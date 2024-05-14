/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <variant>

#include <clang/AST/DeclBase.h>
#include <clang/AST/DeclVisitor.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Index/USRGeneration.h>
#include <clang/Sema/SemaConsumer.h>
#include <llvm/Config/llvm-config.h>
#include <llvm/Support/SHA1.h>
#include <re2/re2.h>

#include "folly/MapUtil.h"
#include "folly/Overload.h"
#include "folly/ScopeGuard.h"
#include "folly/lang/Assume.h"
#include <folly/json/json.h>
#include "glean/lang/clang/ast.h"
#include "glean/lang/clang/index.h"

// This file implements the Clang AST traversal.

namespace {

using namespace facebook::glean::clangx;
using namespace facebook::glean::cpp;
using namespace re2;

template<typename T> T identity(T x) { return x; }

/// Utility function to get Unified Symbol Resolution (USR) Hash for a Decl
/// Creates SHA1 hash of USR and uses 8 bytes of which as Index. This is same
/// as what is being done in clangd. This would allow clangd to use Glean
/// index as a global Index.
/// We currently keep hex string (16bit encoding). We could use 64bit encoding
/// to reduce the size further.
//
std::optional<std::string> getUsrHash(const clang::Decl *decl){
  llvm::SmallString<32> usr;
  if (clang::index::generateUSRForDecl(decl, usr)) {
    return {};
  }
  // `generateUSRForDecl` can return false for reasons such as:
  //
  //   1. `Decl` is `nullptr`
  //   2. anonymous bit fields
  //   3. `ParmDecl` with no name for declaration of a function pointer type
  //      such as `void (*f)(void *)`;
  //   4. USRs for linkage specs themselves etc.
  //
  // These cases does not really matter much to us and not going to lead to any
  // false positives. Also, Glean and clangd uses the same implementation.
  auto hash = llvm::SHA1::hash(llvm::arrayRefFromStringRef(usr));
  return llvm::toHex(llvm::ArrayRef(hash.data(), 8));
}

/// Track usage of using declarations
class UsingTracker {
public:
  explicit UsingTracker(const clang::DeclContext *context, ClangDB& d)
    : globalContext(getCanonicalDeclContext(context))
    , currentContext(CHECK_NOTNULL(globalContext))
    , db(d) {}

  void addNamespace(const clang::NamespaceDecl *decl) {
    if (decl->isAnonymousNamespace() || decl->isInline()) {
      auto parent = getCanonicalDeclContext(decl->getParent());
      auto context = getCanonicalDeclContext(decl);
      forwards[parent].push_back(
        Forward{decl->getBeginLoc(), context, folly::none});

    }
  }

  /// Add a UsingDecl to the current DeclContext
  void addUsingDecl(
      const clang::UsingDecl *decl, Fact<Cxx::UsingDeclaration> fact) {
    // TODO: We don't handle class-scope UsingDecls for now as we'd have to
    // deal with inheritance for that.
    if (!clang::isa<clang::RecordDecl>(decl->getDeclContext())) {
      const auto context = getCanonicalDeclContext(decl->getDeclContext());
      for (const auto *shadow : decl->shadows()) {
        if (const auto target = shadow->getTargetDecl()) {
          if (auto canonical = clang::dyn_cast<clang::NamedDecl>(
              target->getCanonicalDecl())) {
            if (auto tpl = clang::dyn_cast<clang::RedeclarableTemplateDecl>(
                  canonical)) {
              canonical = tpl->getTemplatedDecl();
            }
            usingDecls.insert({{context, canonical}, {decl, fact}});
          }
        }
      }
    }
  }

  void addUsingDirective(
      const clang::UsingDirectiveDecl *decl, Fact<Cxx::UsingDirective> fact) {
    if (auto context = getCanonicalDeclContext(decl->getDeclContext())) {
      if (auto dir_context =
            getCanonicalDeclContext(
              clang::dyn_cast<clang::DeclContext>(
                decl->getNominatedNamespace()))) {
        forwards[context].push_back(
          Forward{decl->getUsingLoc(), dir_context, fact});
      }
    }
  }


  /// Given a NamedDecl and its XRefTarget, add XRefTarget.indirect if the xref
  /// goes through using declarations
  Cxx::XRefTarget retarget(
      const clang::Decl * FOLLY_NULLABLE base,
      Cxx::XRefTarget target) {
    if (base) {
      if (const auto decl =
            clang::dyn_cast_or_null<clang::NamedDecl>(
              base->getCanonicalDecl())) {
        const auto declContext =
          getCanonicalDeclContext(decl->getDeclContext());
        if (declContext != currentContext) {
          const clang::DeclContext *parentContext = nullptr;
          // For non-scoped enumerators, we need to take into account that
          // using the enclosing namespace of the enum type also brings the
          // enumerators into scope:
          //
          // namespace N1 { enum E { A } };
          // namespace N2 { using namespace N1; /* A is in scope */ }
          // namespace N3 { using N1::E; /* A isn't in scope */ }
          //
          // So we have to look for the DeclContext of the enum (but not for
          // the enum itself).
          if (auto enm =
                clang::dyn_cast_or_null<clang::EnumDecl>(declContext)) {
            if (!enm->isScoped()) {
              parentContext = getCanonicalDeclContext(
                enm->getCanonicalDecl()->getDeclContext());
            }
          }
          if (parentContext != currentContext) {
            LookupState state{decl, declContext, parentContext, {}, {}};
            lookup(currentContext, state);
            for (const auto& via : state.via) {
              target = Cxx::XRefTarget::indirect(
                db.fact<Cxx::XRefIndirectTarget>(via, target));
            }
          }
        }
      }
    }
    return target;
  }

private:
  struct LookupState {
    /// The canonical decl we're looking for
    const clang::NamedDecl *decl;

    /// The canonical context of the decl
    const clang::DeclContext *context;

    /// The context of the parent enum decl if we are looking for an enumerator
    const clang::DeclContext * FOLLY_NULLABLE parentContext;

    /// List of XRefVia populated by the lookup
    std::list<Cxx::XRefVia> via;

    /// Visited contexts populated by the lookup
    folly::F14FastSet<const clang::DeclContext *> visited;
  };

  folly::Optional<const clang::DeclContext *> lookupIn(
      const clang::DeclContext * context, LookupState& s) {
    if (s.visited.find(context) == s.visited.end()) {
      s.visited.insert(context);
      if (context == s.context || context == s.parentContext) {
        return nullptr;
      } else if (auto r = folly::get_optional(usingDecls, {context, s.decl})) {
        s.via.push_front(Cxx::XRefVia::usingDeclaration(r->second));
        // Follow chains of using decls - for instance:
        //
        // namespace foo { using std::vector; }
        // namespace bar { using foo::vector; }
        // bar::vector
        //
        // TODO: We should probably cache this rather than recomputing every
        // time.
        return getSpecifierContext(r->first->getQualifier());
      } else {
        auto p = forwards.find(context);
        if (p != forwards.end()) {
          // If we find a decl through a using directive we'll have to place it
          // in the right spot in the via list so remember the current position.
          auto pos = s.via.begin();
          for (auto i = p->second.rbegin(); i != p->second.rend(); ++i) {
            if (auto ctx = lookupIn(i->context, s)) {
              if (i->fact) {
                s.via.insert(
                  pos, Cxx::XRefVia::usingDirective(i->fact.value()));
              }
              return ctx;
            }
          }
        }
        return folly::none;
      }
    } else {
      return folly::none;
    }
  }

  void lookup(const clang::DeclContext *context, LookupState& s) {
    while (context) {
      if (s.visited.find(context) != s.visited.end()) {
        // We might have visited this context via a using directive so we still
        // need to go up. Example:
        //
        // namespace parent {
        // namespace child {
        //   using namespace parent;
        // }
        // }
        context = context->getLookupParent();
      } else if (auto ctx = lookupIn(context, s)) {
        context = ctx.value();
      } else {
        context = context->getLookupParent();
      }
      if (context) {
        context = getCanonicalDeclContext(context);
      }
    }
  }

public:
  // Execute f in a new DeclContext
  template<typename F>
  inline
  auto inContext(const clang::DeclContext * FOLLY_NULLABLE context, F&& f) {
    context = getCanonicalDeclContext(context);
    if (context) {
      std::swap(context, currentContext);
    }
    SCOPE_EXIT {
      if (context) {
        currentContext = context;
      }
    };
    return std::forward<F>(f)();
  }

  // Execute f in the context of a NestedNameSpecifier. Consider:
  //
  // namespace foo { struct T { typedef int U; }; }
  // namespace bar { using foo::T; }
  // bar::T::U x;
  //
  // Here, the xref to U won't go through a using declaration but the xref to T
  // will so we need to make sure the xref is computed in the DeclContext of
  // bar.
  template<typename F>
  inline
  auto inNameContext(
      const clang::NestedNameSpecifier * FOLLY_NULLABLE spec, F&& f) {
    // Consider:
    //
    // namespace foo { struct T { typedef int U; }; }
    // namespace bar { using foo::T; T::U x; }
    //
    // Here, we need to make sure that the xref to T is computed in the
    // enclosing DeclContext but we can't get that from the NestedNameSpecifier.
    // So when we enter a NestedNameSpecified (the first call to inNameContext,
    // savedContext == nullptr), we store the enclosingContext in savedContext.
    // Then, when NestedNameSpecifier is null (i.e., we're about to visit the
    // left-most name), we grab the original context from savedContext.
    //
    // TODO: This is very hacky, make it better.
    //
    auto saved = savedContext;
    SCOPE_EXIT {
      savedContext = saved;
    };
    if (savedContext == nullptr) {
      savedContext = currentContext;
    }
    return inContext(
        spec ? getSpecifierContext(spec) : savedContext, std::forward<F>(f));
  }

  // We can't traverse the entire declaration in the context of the
  // function as this is the wrong context for the return type and for the
  // function's qualified name. So just remember the current function
  // and change the context when traversing the body (via maybeBody).
  template<typename F>
  inline
  auto inFunction(const clang::FunctionDecl *fun, F&& f) {
    auto saved = currentFunction;
    currentFunction = fun;
    SCOPE_EXIT {
      currentFunction = saved;
    };
    return std::forward<F>(f)();
  }

  // Traverse function bodies in the context of the function.
  //
  // Note that the body isn't necessarily the first statement we see after
  // the function decl:
  //
  // Foo::Foo() : bar([] { ... }) { ... }
  template<typename F>
  inline
  auto maybeBody(const clang::Stmt *body, F&& f) {
    if (currentFunction && body == currentFunction->getBody()) {
      return inContext(currentFunction, std::forward<F>(f));
    } else {
      return std::forward<F>(f)();
    }
  }

  const clang::DeclContext * FOLLY_NULLABLE getSpecifierContext(
      const clang::NestedNameSpecifier * FOLLY_NULLABLE spec) {
    if (spec) {
      if (auto ns = spec->getAsNamespace()) {
        return ns;
      } else if (auto rec = spec->getAsRecordDecl()) {
        return rec;
      } else if (spec->getKind()
                  == clang::NestedNameSpecifier::SpecifierKind::Global) {
        return globalContext;
      }
    }
    return nullptr;
  }

private:
  static const clang::DeclContext * FOLLY_NULLABLE getCanonicalDeclContext(
      const clang::DeclContext * FOLLY_NULLABLE ctx) {
    if (ctx) {
      return clang::dyn_cast<clang::DeclContext>(
        clang::dyn_cast<clang::Decl>(ctx)->getCanonicalDecl());
    } else {
      return nullptr;
    }
  }

  const clang::DeclContext * const globalContext;
  const clang::DeclContext *currentContext;
  const clang::DeclContext * FOLLY_NULLABLE savedContext = nullptr;
  const clang::FunctionDecl * FOLLY_NULLABLE currentFunction = nullptr;
  ClangDB& db;
  folly::F14FastMap<
    std::pair<const clang::DeclContext *, const clang::NamedDecl *>,
    std::pair<const clang::UsingDecl *, Fact<Cxx::UsingDeclaration>>>
      usingDecls;

  struct Forward {
    const clang::SourceLocation loc;
    const clang::DeclContext *context;
    folly::Optional<Fact<Cxx::UsingDirective>> fact;
  };

  folly::F14FastMap<const clang::DeclContext *, std::vector<Forward>> forwards;
};

class DeclarationTargets {
  public:
  explicit DeclarationTargets(ClangDB& d) : db(d) {}

  template <typename F>
  auto inDeclaration(Cxx::Declaration decl, F&& f) {
    std::set<DeclRef> savedRefs = std::move(refsInContext);
    SCOPE_EXIT {
      if (!refsInContext.empty()) {
        std::vector<Cxx::Declaration> decls;
        decls.reserve(refsInContext.size());
        for (const auto& [decl, _] : refsInContext) {
            decls.push_back(decl);
        }
        db.fact<Cxx::DeclarationTargets>(decl, std::move(decls));
      }
      refsInContext = std::move(savedRefs);
    };
    return std::forward<F>(f)();
  }

  void contextXRef(Cxx::Declaration target, ClangDB::SourceRange sort_id) {
    refsInContext.insert({ target, sort_id });
  }

  private:
    struct DeclRef {
      Cxx::Declaration decl;
      ClangDB::SourceRange sort_id;

      bool operator<(const DeclRef& other) const {
        return sort_id < other.sort_id;
      }
      bool operator==(const DeclRef& other) const {
        return sort_id == other.sort_id;
      }
    };

    ClangDB& db;
    std::set<DeclRef> refsInContext;
};

struct ASTVisitor : public clang::RecursiveASTVisitor<ASTVisitor> {
  using Base = clang::RecursiveASTVisitor<ASTVisitor>;

  // Memoization of a function
  template<
    typename Key,
    typename Value,
    Value (ASTVisitor::*Compute)(Key),
    Key (*Transform)(Key) = &identity<Key>>
  struct Memo {
    using key_type = Key;
    using value_type = Value;

    explicit Memo(ASTVisitor& v) : visitor(v) {}

    Value operator()(Key key) {
      auto real_key = Transform(key);
      if (auto r = folly::get_optional(items, real_key)) {
        return r.value();
      } else {
        auto value = (visitor.*Compute)(real_key);
        items.emplace(real_key, value);
        return value;
      }
    }

    folly::F14FastMap<Key, Value> items;
    ASTVisitor& visitor;
  };

  // Memoization of a function which can fail to produce a result
  template<typename Key, typename Value, typename Proj = folly::Identity>
  struct MemoOptional {
    using key_type = Key;
    using value_type = Value;

    explicit MemoOptional(const std::string& t, ASTVisitor& v, Proj p = {})
        : visitor(v), proj(p), tag(t) {}

    template<typename K>
    auto operator()(K key) {
      auto projected = proj(key);
      auto real_key = projected ? projected : key;
      struct Result : Value {
        decltype(real_key) key;
      };
      std::optional<Result> result;
      if (auto r = folly::get_optional(items, real_key)) {
        if (auto s = r.value()) {
          result = Result{s.value(), real_key};
        } else {
          LOG(FATAL) << "MemoOptional (" << tag << "): infinite loop";
        }
      } else {
        items.insert({real_key, folly::none});
        auto v = Value::compute(visitor, real_key);
        if (v) {
          items[real_key] = v;
          result = Result{v.value(), real_key};
        } else {
          items.erase(real_key);
        }
      }
      return result;
    }

    folly::F14FastMap<Key, folly::Optional<Value>> items;
    ASTVisitor& visitor;
    Proj proj;
    const std::string tag;
  };

  // Obtain the FunctionName for a decl unless we choose to ignore it
  folly::Optional<Fact<Cxx::FunctionName>> functionName(
      const clang::NamedDecl *decl) {
    auto name = decl->getDeclName();
    switch (name.getNameKind()) {
      case clang::DeclarationName::Identifier:
        if (auto ide = name.getAsIdentifierInfo()) {
          return db.fact<Cxx::FunctionName>(
            alt<0>(db.name(ide->getName())));
        } else {
          return folly::none;
        }

      case clang::DeclarationName::ObjCZeroArgSelector:
        return folly::none;

      case clang::DeclarationName::ObjCOneArgSelector:
        return folly::none;

      case clang::DeclarationName::ObjCMultiArgSelector:
        return folly::none;

      case clang::DeclarationName::CXXOperatorName:
        return db.fact<Cxx::FunctionName>(
          alt<1>(name.getAsString()));

      case clang::DeclarationName::CXXLiteralOperatorName:
        return db.fact<Cxx::FunctionName>(
          alt<2>(name.getAsString()));

      case clang::DeclarationName::CXXConstructorName:
        return db.fact<Cxx::FunctionName>(
          alt<3>(std::make_tuple()));

      case clang::DeclarationName::CXXDestructorName:
        return db.fact<Cxx::FunctionName>(
          alt<4>(std::make_tuple()));

      case clang::DeclarationName::CXXConversionFunctionName: {
        auto cd = clang::dyn_cast<clang::CXXConversionDecl>(decl);
        return db.fact<Cxx::FunctionName>(
          alt<5>(type(cd ? cd->getConversionType() : name.getCXXNameType())));
      }

      case clang::DeclarationName::CXXDeductionGuideName:
        return folly::none;

      case clang::DeclarationName::CXXUsingDirective:
        return folly::none;
    }
    folly::assume_unreachable();
  }


  /**********
   * Scopes *
   **********/


  // Scopes: global, namespace, class + access
  struct GlobalScope {};

  struct NamespaceScope {
    const clang::NamespaceDecl *decl;
    Fact<Cxx::NamespaceQName> fact;
  };

  struct ClassScope {
    const clang::RecordDecl *decl;
    Fact<Cxx::QName> fact;
  };

  struct LocalScope {
    const clang::FunctionDecl *decl;
    Fact<Cxx::FunctionQName> fact;
  };

  using Scope = std::variant<
    GlobalScope,
    NamespaceScope,
    ClassScope,
    LocalScope>;

  // Translate clang::AccessSpecifier to cxx.Access
  static Cxx::Access access(clang::AccessSpecifier spec) {
    switch (spec) {
      case clang::AS_public:
        return Cxx::Access::Public;

      case clang::AS_protected:
        return Cxx::Access::Protected;

      case clang::AS_private:
        return Cxx::Access::Private;

      default:
        // TODO: is this always public?
        return Cxx::Access::Public;
    }
  }

  // Obtain the scope for a DeclContext
  Scope defineScope(const clang::DeclContext *ctx) {
    while (ctx) {
      if (clang::isa<clang::TranslationUnitDecl>(ctx)) {
        return GlobalScope{};
      } else if (auto x = clang::dyn_cast<clang::NamespaceDecl>(ctx)) {
        if (auto r = namespaces(x)) {
          return NamespaceScope{r->key, r->qname};
        }
      } else if (auto x = clang::dyn_cast<clang::CXXRecordDecl>(ctx)) {
        if (auto r = classDecls(x)) {
          return ClassScope{r->key, r->qname};
        }
      } else if (auto x = clang::dyn_cast<clang::FunctionDecl>(ctx)) {
        if (auto r = funDecls(x)) {
          return LocalScope{r->key, r->qname};
        }
      }
      ctx = ctx->getParent();
    }
    return GlobalScope{};
  }

  // Obtain the parent scope of a Decl.
  Scope parentScope(const clang::Decl *decl) {
    return scopes(decl->getDeclContext());
  }

  Cxx::Scope scopeRepr(const Scope& scope, clang::AccessSpecifier acs) {
    return folly::variant_match(
        scope,
        [](const GlobalScope&) { return Cxx::Scope::global_(); },
        [](const NamespaceScope& ns) {
          return Cxx::Scope::namespace_(ns.fact);
        },
        [acs](const ClassScope& cls) {
          return Cxx::Scope::recordWithAccess(cls.fact, access(acs));
        },
        [](const LocalScope& fun) { return Cxx::Scope::local(fun.fact); });
  }

  // Obtain the cxx.Scope of a Decl and translate it to clang.Scope
  Cxx::Scope parentScopeRepr(const clang::Decl *decl) {
    return scopeRepr(parentScope(decl), decl->getAccess());
  }


  struct DeclTraits {
    static bool isImplicit(const clang::Decl* decl) {
      if (!decl->isImplicit()) {
        return false;
      }
      // ObjC getters/setters are produced by corresponding ObjC property decls.
      if (clang::dyn_cast<clang::ObjCMethodDecl>(decl)) {
        return false;
      }
      auto rd = clang::dyn_cast<clang::CXXRecordDecl>(decl);
      return !(rd && rd->isLambda());
    }

    template<typename T>
    static bool isDefinition(const T* decl) {
      return DeclTraits::getDefinition(decl) == decl;
    }

    static bool isDefinition(const clang::ObjCMethodDecl *decl) {
      return decl->isThisDeclarationADefinition();
    }

    template<typename T>
    static const T *getDefinition(const T *decl) {
      return decl->getDefinition();
    }

    static const clang::NamespaceDecl * FOLLY_NULLABLE getDefinition(
        const clang::NamespaceDecl *decl) {
      return decl;
    }

    static const clang::TypedefNameDecl * FOLLY_NULLABLE getDefinition(
        const clang::TypedefNameDecl *) {
      return nullptr;
    }

    static const clang::NamespaceAliasDecl* FOLLY_NULLABLE
    getDefinition(const clang::NamespaceAliasDecl*) {
      return nullptr;
    }

    static const clang::FieldDecl * FOLLY_NULLABLE getDefinition(
        const clang::FieldDecl *) {
      return nullptr;
    }

    static const clang::ObjCCategoryDecl *getDefinition(
        const clang::ObjCCategoryDecl *decl) {
      return decl;    // TODO: is this right?
    }

    static const clang::ObjCImplementationDecl *getDefinition(
        const clang::ObjCImplementationDecl *decl) {
      return decl;    // TODO: is this right?
    }

    static const clang::ObjCCategoryImplDecl *getDefinition(
        const clang::ObjCCategoryImplDecl *decl) {
      return decl;    // TODO: is this right?
    }

    static const clang::ObjCMethodDecl * FOLLY_NULLABLE getDefinition(
        const clang::ObjCMethodDecl *) {
      return nullptr;
    }

    static const clang::ObjCPropertyDecl * FOLLY_NULLABLE getDefinition(
        const clang::ObjCPropertyDecl *) {
      return nullptr;
    }

    template<typename T>
    static const T *getCanonicalDecl(const T *decl) {
      return decl->getCanonicalDecl();
    }

    static const clang::ObjCCategoryDecl *getCanonicalDecl(
        const clang::ObjCCategoryDecl *decl) {
      return decl;    // TODO: is this right?
    }

    static const clang::ObjCImplementationDecl *getCanonicalDecl(
        const clang::ObjCImplementationDecl *decl) {
      return decl;    // TODO: is this right?
    }

    static const clang::ObjCCategoryImplDecl *getCanonicalDecl(
        const clang::ObjCCategoryImplDecl *decl) {
      return decl;    // TODO: is this right?
    }

    static const clang::ObjCPropertyDecl *getCanonicalDecl(
        const clang::ObjCPropertyDecl *decl) {
      return decl;    // TODO: is this right?
    }

    // Given an instantiated declaration, returns the corresponding templated
    // declaration and `nullptr` otherwise. The semantics of this function
    // mimics `getTemplateInstantiationPattern` that exist for other entities.
    //
    // This provides the functionality for entities such as `TypedefDecl` and
    // `FieldDecl`. These are entities that can be non-templates within a class
    // template and therefore are still instantiated.
    //
    // For example:
    //
    // ```
    // template <typename T>
    // struct S {
    //   using X = T;
    //   T t;
    // };
    //
    // S<int>::X x;  // type alias ref
    // S<int> s;
    // some_fn(s.t); // field decl ref
    // ```
    //
    // In this scenario, `S<int>::X` is a type alias where the corresponding
    // type is `int`. While this is correct, in the indexer we want to keep
    // track of the "base template", i.e., `S<T>::X`.
    static const clang::TypedefNameDecl* FOLLY_NULLABLE
    getTemplateInstantiationPattern(const clang::TypedefNameDecl* decl) {
      return getTemplateInstantiationPatternImpl(decl);
    }

    static const clang::FieldDecl* FOLLY_NULLABLE
    getTemplateInstantiationPattern(const clang::FieldDecl* decl) {
      return decl->getDeclName()
          ? getTemplateInstantiationPatternImpl(decl)
          : decl->getASTContext().getInstantiatedFromUnnamedFieldDecl(
                const_cast<clang::FieldDecl*>(decl));
    }

   private:
    template <typename T>
    static const T* FOLLY_NULLABLE
    getTemplateInstantiationPatternImpl(const T* decl) {
      if (auto rd =
              clang::dyn_cast<clang::CXXRecordDecl>(decl->getDeclContext())) {
        if (auto tpl = rd->getTemplateInstantiationPattern()) {
          if (auto result = tpl->lookup(decl->getDeclName()); !result.empty()) {
            return clang::dyn_cast<T>(result.front());
          }
        }
      }
      return nullptr;
    }

   public:
    template<typename T>
    static constexpr bool canHaveComments(const T *) {
      return true;
    }

    static constexpr bool canHaveComments(const clang::NamespaceDecl *) {
      // Clang assigns comments at the top level of a module to the first
      // namespace decls. Comments on namespaces probably aren't interesting,
      // anyway.
      return false;
    }
    };

  template<typename Memo, typename Decl>
  static typename Memo::value_type representative(
      Memo& memo,
      const Decl *decl,
      typename Memo::value_type me) {
    auto defn = DeclTraits::getDefinition(decl);
    if (defn == decl) {
      return me;
    } else if (defn != nullptr) {
      if (auto r = memo(defn)) {
        return r.value();
      }
    }

    auto can = DeclTraits::getCanonicalDecl(decl);
    if (can != nullptr) {
      if (auto r = memo(can)) {
        return r.value();
      }
    }

    return me;
  }

  template<typename Decl>
  struct Declare {
    template<typename ClangDecl>
    static folly::Optional<Decl> compute(
        ASTVisitor& visitor,
        const ClangDecl *decl) {
      if (DeclTraits::isImplicit(decl)) {
        return folly::none;
      }
      auto range = visitor.db.srcRange(decl->getSourceRange());
      folly::Optional<Decl> result =
        Decl::declare(
          visitor, decl, visitor.parentScopeRepr(decl), range.range);
      if (result) {
        visitor.db.declaration(range, result->declaration());
        // Reference to `nameLocation` function in ClangD:
        // https://github.com/llvm/llvm-project/blob/main/clang-tools-extra/clangd/AST.cpp
        const auto nameLocation = [&] {
          if (const auto* method =
                  clang::dyn_cast<clang::ObjCMethodDecl>(decl)) {
            return method->getSelectorStartLoc();
          }
          return decl->getLocation();
        }();
        const auto nameRange = folly::variant_match(
            visitor.db.fullSrcRange(nameLocation),
            [](const ClangDB::SourceRange& range) { return range; },
            [](const auto& range) {
              const auto& [expansion, spelling] = range;
              return spelling && spelling->file ? *spelling : expansion;
            });
        if (nameRange.file) {
          visitor.db.fact<Cxx::DeclarationNameSpan>(
              result->declaration(), nameRange.file->fact, nameRange.span);
        }
        if (DeclTraits::canHaveComments(decl)){
          if (auto comment =
                decl->getASTContext().getRawCommentForDeclNoCache(decl)) {
            auto crange = visitor.db.srcRange(comment->getSourceRange());
            visitor.db.fact<Cxx::DeclarationComment>(
              result->declaration(),
              crange.range.file,   // might be "<builtin>"
              crange.span);

            // Some comments represent thrift generated functions. Their
            // content is used to generate Cxx1.CxxToThrift facts.

            std::string thrift_file;
            std::string thrift_service;
            std::string thrift_function;
            std::string json_annotation;
            auto comment_str = comment->getRawText(visitor.db.sourceManager()).str();
            static const RE2 thrift_regex(
              R"(Glean.*({.*}))"
            );
            if (RE2::PartialMatch(comment_str, thrift_regex, &json_annotation)) {
               try {
                 folly::dynamic jsonObject = folly::parseJson(json_annotation);
                 thrift_file = jsonObject["file"].asString();
                 if (visitor.db.cell.has_value()) {
                    thrift_file = visitor.db.cell.value() + "/" + thrift_file;
                 }
                 thrift_service = jsonObject["service"].asString();
                 thrift_function = jsonObject["function"].asString();

                 auto qual_name = visitor.db.fact<Fbthrift::QualName>(
                   visitor.db.fact<Fbthrift::File>(visitor.db.fact<Src::File>(thrift_file)),
                   visitor.db.fact<Fbthrift::Identifier>(thrift_service)
                 );
                 auto function_name = visitor.db.fact<Fbthrift::FunctionName>(
                   visitor.db.fact<Fbthrift::ServiceName>(qual_name),
                   visitor.db.fact<Fbthrift::Identifier>(thrift_function)
                 );

                 visitor.db.fact<Cxx::CxxToThrift>(
                   Cxx::XRefTarget::declaration(result->declaration()),
                   Fbthrift::XRefTarget::function_(function_name)
                 );
               } catch (...) {
               }
            };
          }
        }
      }
      return result;
    }
  };

  template<typename Memo, typename Decl>
  void visitDeclaration(Memo& memo, const Decl *decl) {
    if (auto mdecl = memo(decl)) {
      decl = mdecl->key;
      if (DeclTraits::isDefinition(decl)) {
        mdecl->define(*this, decl);
      }
      auto same = representative(memo, decl, mdecl.value());
      const auto this_decl = mdecl->declaration();
      const auto other_decl = same.declaration();
      if (this_decl != other_decl) {
        db.fact<Cxx::Same>(this_decl, other_decl);
      }
    }
  }

  static std::vector<Cxx::Declaration> members(
      ASTVisitor& visitor,
      const clang::DeclContext* d) {
    std::vector<Cxx::Declaration> members;
    auto add = [&members](auto& memo, const auto* decl) {
      if (auto m = memo(decl)) {
        members.push_back(m->declaration());
      }
    };
    for (const auto& mem : d->decls()) {
      if (auto record = clang::dyn_cast<clang::CXXRecordDecl>(mem)) {
        if (!record->isInjectedClassName()) {
          add(visitor.classDecls, record);
        }
      } else if (auto ctd = clang::dyn_cast<clang::ClassTemplateDecl>(mem)) {
        auto rec = ctd->getTemplatedDecl();
        if (rec && !rec->isInjectedClassName()) {
          add(visitor.classDecls, rec);
        }
      } else if (auto fun = clang::dyn_cast<clang::FunctionDecl>(mem)) {
        add(visitor.funDecls, fun);
      } else if (auto ftd = clang::dyn_cast<clang::FunctionTemplateDecl>(mem)) {
        add(visitor.funDecls, ftd->getTemplatedDecl());
      } else if (auto ed = clang::dyn_cast<clang::EnumDecl>(mem)) {
        add(visitor.enumDecls, ed);
      } else if (auto fd = clang::dyn_cast<clang::FieldDecl>(mem)) {
        add(visitor.varDecls, fd);
      } else if (auto vd = clang::dyn_cast<clang::VarDecl>(mem)) {
        // See `VisitVarDecl` for why we need to do this.
        if (!clang::isTemplateInstantiation(
                vd->getTemplateSpecializationKind())) {
          add(visitor.varDecls, vd);
        }
      } else if (auto vtd = clang::dyn_cast<clang::VarTemplateDecl>(mem)) {
        add(visitor.varDecls, vtd->getTemplatedDecl());
      } else if (auto tnd = clang::dyn_cast<clang::TypedefNameDecl>(mem)) {
        add(visitor.typeAliasDecls, tnd);
      } else if (
          auto tatd = clang::dyn_cast<clang::TypeAliasTemplateDecl>(mem)) {
        add(visitor.typeAliasDecls, tatd->getTemplatedDecl());
      } else if (auto ns = clang::dyn_cast<clang::NamespaceDecl>(mem)) {
        add(visitor.namespaces, ns);
      } else if (auto ns = clang::dyn_cast<clang::NamespaceAliasDecl>(mem)) {
        add(visitor.namespaceAliasDecls, ns);
      }
    }
    return members;
  }

  /**************
   * Namespaces *
   **************/

  // Obtain the parent namespace of a Decl, if any
  folly::Optional<Fact<Cxx::NamespaceQName>> parentNamespace(
      const clang::Decl* decl) {
    return folly::variant_match<folly::Optional<Fact<Cxx::NamespaceQName>>>(
        parentScope(decl),
        [](const GlobalScope&) { return folly::none; },
        [](const NamespaceScope& ns) { return ns.fact; },
        [](const ClassScope&) {
          LOG(ERROR) << "Inner scope is a class, should have been a namespace";
          return folly::none;
        },
        [](const LocalScope&) {
          LOG(ERROR) << "Inner scope is a function, should have been a namespace";
          return folly::none;
        });
  }

  struct NamespaceDecl : Declare<NamespaceDecl> {
    Fact<Cxx::NamespaceQName> qname;
    Fact<Cxx::NamespaceDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::namespace_(decl);
    }

    static folly::Optional<NamespaceDecl> declare(
        ASTVisitor& visitor,
        const clang::NamespaceDecl *decl,
        Cxx::Scope,
        Src::Range range) {
      folly::Optional<Fact<Cxx::Name>> name;
      if (!decl->isAnonymousNamespace()) {
        name = visitor.db.name(decl->getName());
      }

      auto qname = visitor.db.fact<Cxx::NamespaceQName>(
        maybe(name), maybe(visitor.parentNamespace(decl)));

      return NamespaceDecl
        { {}
        , qname
        , visitor.db.fact<Cxx::NamespaceDeclaration>(qname, range)
        };
    }

    void define(ASTVisitor& visitor, const clang::NamespaceDecl *d) const {
      visitor.db.fact<Cxx::NamespaceDefinition>(
          decl, visitor.db.fact<Cxx::Declarations>(members(visitor, d)));
    }
  };

  // Clang namespace visitor
  bool VisitNamespaceDecl(clang::NamespaceDecl *decl) {
    usingTracker.addNamespace(decl);
    visitDeclaration(namespaces, decl);
    return true;
  }

  /**********************
   * Using declarations *
   **********************/

  bool VisitUsingDecl(const clang::UsingDecl *decl) {
    if (auto name = functionName(decl)) {
      if (auto context =
          usingTracker.getSpecifierContext(decl->getQualifier())) {
        auto range = db.srcRange(decl->getSourceRange());
        auto fact = db.fact<Cxx::UsingDeclaration>(
          db.fact<Cxx::FunctionQName>(
            name.value(),
            scopeRepr(scopes(context), decl->getAccess())),
          range.range);
        db.declaration(range, Cxx::Declaration::usingDeclaration(fact));
        usingTracker.addUsingDecl(decl, fact);
      }
      for (const auto* shadow : decl->shadows()) {
        xrefExpr(
            decl->getNameInfo().getSourceRange(), decl->getQualifier(), shadow);
      }
    }
    return true;
  }

  /********************
   * Using directives *
   ********************/

  bool VisitUsingDirectiveDecl(const clang::UsingDirectiveDecl *decl) {
    if (auto nominated = decl->getNominatedNamespace()) {
      xrefExpr(decl->getIdentLocation(), decl->getQualifier(), nominated);
    }
    if (auto ns = decl->getNominatedNamespaceAsWritten()) {
      auto range = db.srcRange(decl->getSourceRange());
      auto fact = db.fact<Cxx::UsingDirective>(
          db.fact<Cxx::QName>(
            db.name(ns->getName()),
            parentScopeRepr(ns)),
          range.range);
      db.declaration(range, Cxx::Declaration::usingDirective(fact));
      usingTracker.addUsingDirective(decl, fact);
    }
    return true;
  }

  /***********
   * Enums *
   ***********/

  Fact<Cxx::Enumerator> enumerator(
      Fact<Cxx::EnumDeclaration> type,
      const clang::EnumConstantDecl *decl) {
    return db.fact<Cxx::Enumerator>(
      db.name(decl->getName()),
      type,
      db.srcRange(decl->getSourceRange()).range);
  }

  struct EnumDecl : Declare<EnumDecl> {
    Fact<Cxx::EnumDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::enum_(decl);
    }

    static folly::Optional<EnumDecl> declare(
        ASTVisitor& visitor,
        const clang::EnumDecl *decl,
        Cxx::Scope scope,
        Src::Range range) {
      auto qname = visitor.db.fact<Cxx::QName>(
        visitor.db.name(decl->getName()), scope);

      folly::Optional<Fact<Cxx::Type>> underlying;
      if (auto ty = decl->getIntegerTypeSourceInfo()) {
        underlying = visitor.type(ty->getType());
      }

      return EnumDecl
        { {}
        , visitor.db.fact<Cxx::EnumDeclaration>(
            qname,
            decl->isScoped(),
            maybe(underlying),
            range)
        };
    }

    void define(ASTVisitor& visitor, const clang::EnumDecl *d) const {
      std::vector<Fact<Cxx::Enumerator>> enumerators;
      for (const auto& e : d->enumerators()) {
        enumerators.push_back(visitor.enumerator(decl, e));
      }

      if (auto usr_hash = getUsrHash(d)){
        visitor.db.fact<Cxx::USRToDeclaration>(usr_hash.value(),
            Cxx::Declaration::enum_(decl));
      }

      visitor.db.fact<Cxx::EnumDefinition>(decl, enumerators);
    }

    struct GetTemplatableDecl {
      const clang::EnumDecl* FOLLY_NULLABLE
      operator()(const clang::EnumDecl* decl) const {
        return decl->getTemplateInstantiationPattern();
      }
    };
  };

  struct EnumeratorDecl {
    Fact<Cxx::Enumerator> fact;

    static folly::Optional<EnumeratorDecl> compute(
        ASTVisitor& visitor,
        const clang::EnumConstantDecl *decl) {
      if (auto ty = clang::dyn_cast<clang::EnumDecl>(decl->getDeclContext())) {
        if (auto enm = visitor.enumDecls(ty)) {
          return EnumeratorDecl{ visitor.enumerator(enm->decl, decl) };
        }
      }
      return folly::none;
    }
  };

  bool VisitEnumDecl(const clang::EnumDecl *decl) {
    visitDeclaration(enumDecls, decl);
    return true;
  }


  /****************
   * Type aliases *
   ****************/

  struct TypeAliasDecl : Declare<TypeAliasDecl> {
    Fact<Cxx::TypeAliasDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::typeAlias(decl);
    }

    static folly::Optional<TypeAliasDecl> declare(
        ASTVisitor& visitor,
        const clang::TypedefNameDecl *decl,
        Cxx::Scope scope,
        Src::Range range) {
      folly::Optional<Cxx::TypeAliasKind> kind;
      if (clang::isa<clang::TypeAliasDecl>(decl)) {
        kind = Cxx::TypeAliasKind::Using;
      } else if (clang::isa<clang::TypedefDecl>(decl)) {
        kind = Cxx::TypeAliasKind::Typedef;
      }
      if (kind) {
        auto qname = visitor.db.fact<Cxx::QName>(
          visitor.db.name(decl->getName()), scope);
        auto type = visitor.type(decl->getUnderlyingType());
        return TypeAliasDecl
          { {}
          , visitor.db.fact<Cxx::TypeAliasDeclaration>(
              qname,
              type,
              kind.value(),
              range)
          };
      } else {
        return folly::none;
      }
    }

    void define(ASTVisitor&, const clang::TypedefNameDecl *) const {}

    struct GetTemplatableDecl {
      const clang::TypedefNameDecl* FOLLY_NULLABLE
      operator()(const clang::TypedefNameDecl* decl) const {
        if (auto td = DeclTraits::getTemplateInstantiationPattern(decl)) {
          return td;
        }
        if (auto ta = clang::dyn_cast<clang::TypeAliasDecl>(decl)) {
          // If this is the pattern of a type alias template, find where it was
          // instantiated from.
          if (auto ct = ta->getDescribedAliasTemplate()) {
            // If we hit a point where the user provided a specialization of this
            // template, we're done looking.
            while (auto next = ct->getInstantiatedFromMemberTemplate()) {
              ct = next;
            }
            if (auto cd = ct->getTemplatedDecl(); cd && cd != ta) {
              return cd;
            }
          }
        }
        return nullptr;
      }
    };
  };

  bool VisitTypedefNameDecl(const clang::TypedefNameDecl *decl) {
    visitDeclaration(typeAliasDecls, decl);
    return true;
  }

  struct NamespaceAliasDecl : Declare<NamespaceAliasDecl> {
    Fact<Cxx::NamespaceAliasDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::namespaceAlias(decl);
    }

    static folly::Optional<NamespaceAliasDecl> declare(
        ASTVisitor& visitor,
        const clang::NamespaceAliasDecl* decl,
        Cxx::Scope,
        Src::Range range) {
      auto qname = visitor.db.fact<Cxx::NamespaceQName>(
          just(visitor.db.name(decl->getName())),
          maybe(visitor.parentNamespace(decl)));
      std::optional<Cxx::NamespaceTarget> target;
      const auto* alias = CHECK_NOTNULL(decl->getAliasedNamespace());
      if (auto x = clang::dyn_cast<clang::NamespaceDecl>(alias)) {
        if (auto r = visitor.namespaces(x)) {
          target = Cxx::NamespaceTarget::namespace_(r->decl);
        }
      } else if (auto x = clang::dyn_cast<clang::NamespaceAliasDecl>(alias)) {
        if (auto r = visitor.namespaceAliasDecls(x)) {
          target = Cxx::NamespaceTarget::namespaceAlias(r->decl);
        }
      }
      if (target) {
        return NamespaceAliasDecl{
            {},
            visitor.db.fact<Cxx::NamespaceAliasDeclaration>(
                qname, std::move(target).value(), range)};
      } else {
        return folly::none;
      }
    }

    void define(ASTVisitor&, const clang::NamespaceAliasDecl *) const {}
  };

  bool VisitNamespaceAliasDecl(clang::NamespaceAliasDecl *decl) {
    visitDeclaration(namespaceAliasDecls, decl);
    xrefExpr(
        decl->getTargetNameLoc(), decl->getQualifier(), decl->getNamespace());
    return true;
  }

  /***********
   * Classes *
   ***********/

  struct ClassDecl : Declare<ClassDecl> {
    Fact<Cxx::QName> qname;
    Fact<Cxx::RecordDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::record_(decl);
    }

    static folly::Optional<ClassDecl> declare(
        ASTVisitor& visitor,
        const clang::CXXRecordDecl *decl,
        Cxx::Scope scope,
        Src::Range range) {
      if (decl->isInjectedClassName()) {
        return folly::none;
      }

      folly::Optional<Cxx::RecordKind> kind;
      switch (decl->getTagKind()) {
#if LLVM_VERSION_MAJOR >= 18
        case clang::TagTypeKind::Struct:
#else
        case clang::TTK_Struct:
#endif
          kind = Cxx::RecordKind::struct_();
          break;

#if LLVM_VERSION_MAJOR >= 18
        case clang::TagTypeKind::Class:
#else
        case clang::TTK_Class:
#endif
          kind = Cxx::RecordKind::class_();
          break;

#if LLVM_VERSION_MAJOR >= 18
        case clang::TagTypeKind::Union:
#else
        case clang::TTK_Union:
#endif
          kind = Cxx::RecordKind::union_();
          break;

        default:
          break;
      }
      if (kind) {
        auto qname = visitor.db.fact<Cxx::QName>(
          visitor.db.name(decl->getName()), scope);
        return ClassDecl
          { {}
          , qname
          , visitor.db.fact<Cxx::RecordDeclaration>(
              qname,
              kind.value(),
              range)
          };
      } else {
        return folly::none;
      }
    }

    void define(ASTVisitor& visitor, const clang::CXXRecordDecl *d) const {
      std::vector<Cxx::RecordBase> bases;
      for (const auto& base : d->bases()) {
        if (auto ty = base.getType().getTypePtrOrNull()) {
          if (auto record = ty->getAsCXXRecordDecl()) {
            if (auto other = visitor.classDecls(record)) {
              bases.push_back(Cxx::RecordBase{
                other->decl,  // should this be base.representative?
                visitor.access(base.getAccessSpecifier()),
                base.isVirtual()
              });
            }
          }
        }
      }

      if (auto usr_hash = getUsrHash(d)){
        visitor.db.fact<Cxx::USRToDeclaration>(usr_hash.value(),
            Cxx::Declaration::record_(decl));
      }

      visitor.db.fact<Cxx::RecordDefinition>(
          decl, bases, visitor.db.fact<Cxx::Declarations>(members(visitor, d)));
    }

    struct GetTemplatableDecl {
      const clang::CXXRecordDecl* FOLLY_NULLABLE
      operator()(const clang::CXXRecordDecl* decl) const {
        if (auto cd = decl->getTemplateInstantiationPattern()) {
          return cd;
        }
        // If this is the pattern of a class template, find where it was
        // instantiated from.
        if (auto ct = decl->getDescribedClassTemplate()) {
          // If we hit a point where the user provided a specialization of this
          // template, we're done looking.
          while (!ct->isMemberSpecialization()) {
            auto next = ct->getInstantiatedFromMemberTemplate();
            if (!next) {
              break;
            }
            ct = next;
          }
          if (auto cd = ct->getTemplatedDecl(); cd && cd != decl) {
            auto def = cd->getDefinition();
            return def ? def : cd;
          }
        }
        return nullptr;
      }
    };

  };

  // Clang record visitor
  bool VisitCXXRecordDecl(const clang::CXXRecordDecl *decl) {
    visitDeclaration(classDecls, decl);
    return true;
  }

  /*************
   * Functions *
   *************/

  Fact<Cxx::Type> type(const clang::QualType& ty) {
    return db.fact<Cxx::Type>(ty.getAsString(astContext.getPrintingPolicy()));
  }

  Fact<Cxx::Signature> signature(
      const clang::QualType& result,
      clang::ArrayRef<clang::ParmVarDecl *> parameters) {
    std::vector<Cxx::Parameter> params;
    for (auto parm : parameters) {
      params.push_back({db.name(parm->getName()), type(parm->getType())});
    }

    return db.fact<Cxx::Signature>(type(result), params);
  }

  static Cxx::RefQualifier refQualifier(clang::RefQualifierKind rq) {
    switch (rq) {
      case clang::RQ_None:
        return Cxx::RefQualifier::None_;

      case clang::RQ_LValue:
        return Cxx::RefQualifier::LValue;

      case clang::RQ_RValue:
        return Cxx::RefQualifier::RValue;

      default:
        LOG(ERROR) << "unknown clang::RefQualifiedKind";
        return Cxx::RefQualifier::None_;
    }
  }

  struct FunDecl : Declare<FunDecl> {
    Fact<Cxx::FunctionQName> qname;
    Fact<Cxx::FunctionDeclaration> decl;
    bool method;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::function_(decl);
    }

    static folly::Optional<FunDecl> declare(
        ASTVisitor& visitor,
        const clang::FunctionDecl *decl,
        Cxx::Scope scope,
        Src::Range range) {
      // TODO: should we ignore deleted functions or have some info about them?
      if (decl->isDeleted()) {
        return folly::none;
      }

      if (auto name = visitor.functionName(decl)) {
        folly::Optional<Cxx::MethodSignature> method;

        if (auto mtd = clang::dyn_cast<clang::CXXMethodDecl>(decl)) {
          if (mtd->isInstance()) {
            method = Cxx::MethodSignature{
              mtd->isVirtual(),
              mtd->isConst(),
              mtd->isVolatile(),
              visitor.refQualifier(mtd->getRefQualifier())
            };
          }
        }

        if (auto ctx = decl->getLexicalDeclContext();
            ctx && ctx->isFunctionOrMethod()) {
          scope = visitor.scopeRepr(visitor.scopes(ctx), decl->getAccess());
        }
        auto qname = visitor.db.fact<Cxx::FunctionQName>(name.value(), scope);
        auto decl_fact = visitor.db.fact<Cxx::FunctionDeclaration>(
          qname,
          visitor.signature(decl->getReturnType(), decl->parameters()),
          maybe(method),
          range
        );

        if (decl->hasAttrs()) {
          for (const auto attr : decl->getAttrs()) {
            visitor.db.fact<Cxx::FunctionAttribute>(
              visitor.db.fact<Cxx::Attribute>(visitor.db.srcText(attr->getRange()).str()),
              decl_fact
            );
          }
        }
        return FunDecl{
            {},
            qname,
            decl_fact,
            method.has_value()};
      } else {
        return folly::none;
      }
    }

    void define(ASTVisitor& visitor, const clang::FunctionDecl *d) const {
      visitor.db.fact<Cxx::FunctionDefinition>(
        decl,
        d->isInlineSpecified());

      if (auto usr_hash = getUsrHash(d)){
        visitor.db.fact<Cxx::USRToDeclaration>(usr_hash.value(),
            Cxx::Declaration::function_(decl));
      }

      if (method) {
        if (auto mtd = clang::dyn_cast<clang::CXXMethodDecl>(d)) {
          for (const auto *base : mtd->overridden_methods()) {
            if (auto cbase = visitor.funDecls(base)) {
              visitor.db.fact<Cxx::MethodOverrides>(decl, cbase->decl);
            }
          }
        }
      }
    }

    struct GetTemplatableDecl {
      const clang::FunctionDecl* FOLLY_NULLABLE
      operator()(const clang::FunctionDecl* decl) const {
        if (auto fd = decl->getTemplateInstantiationPattern()) {
          return fd;
        }
        // If this is the pattern of a function template, find where it was
        // instantiated from.
        if (auto ft = decl->getDescribedFunctionTemplate()) {
          // If we hit a point where the user provided a specialization of this
          // template, we're done looking.
          while (!ft->isMemberSpecialization()) {
            auto next = ft->getInstantiatedFromMemberTemplate();
            if (!next) {
              break;
            }
            ft = next;
          }
          if (auto fd = ft->getTemplatedDecl(); fd && fd != decl) {
            auto def = fd->getDefinition();
            return def ? def : fd;
          }
        }
        return nullptr;
      }
    };

    static const clang::FunctionDecl * FOLLY_NULLABLE getSpecializedDecl(
        const clang::FunctionDecl *decl) {
      auto tpl = decl->getPrimaryTemplate();
      return tpl ? tpl->getTemplatedDecl() : nullptr;
    }
  };

  bool VisitFunctionDecl(const clang::FunctionDecl *decl) {
    visitDeclaration(funDecls, decl);
    return true;
  }

  /*************
   * Variables *
   *************/

  struct VarDecl : Declare<VarDecl> {
    Fact<Cxx::QName> qname;
    Fact<Cxx::VariableDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::variable(decl);
    }

    static std::variant<std::monostate, Cxx::GlobalVariableKind, Cxx::LocalVariableKind> variableKind(
        const clang::VarDecl *decl) {
      if (decl->isLocalVarDeclOrParm()) {
        if (decl->isLocalVarDecl()) {
          return decl->isStaticLocal() ? Cxx::LocalVariableKind::StaticVariable
                                       : Cxx::LocalVariableKind::SimpleVariable;
        } else {
          return Cxx::LocalVariableKind::Parameter;
        }
      }
      if (decl->isStaticDataMember()) {
        return Cxx::GlobalVariableKind::StaticMember;
      }
      switch (decl->getStorageClass()) {
        case clang::SC_None: return Cxx::GlobalVariableKind::SimpleVariable;
        case clang::SC_Extern: return Cxx::GlobalVariableKind::SimpleVariable;
        case clang::SC_Static: return Cxx::GlobalVariableKind::StaticVariable;
        case clang::SC_PrivateExtern: return {};
        case clang::SC_Auto: return {};
        case clang::SC_Register: return {};
      }
      folly::assume_unreachable();
    }

    static Cxx::GlobalVariableAttribute globalAttribute(
        const clang::VarDecl *decl) {
      if (decl->isConstexpr()) {
        return Cxx::GlobalVariableAttribute::Constexpr;
      } else if (decl->isInline()) {
        return Cxx::GlobalVariableAttribute::Inline;
      } else {
        return Cxx::GlobalVariableAttribute::Plain;
      }
    }

    static Cxx::LocalVariableAttribute localAttribute(
        const clang::VarDecl *decl) {
      if (decl->isConstexpr()) {
        return Cxx::LocalVariableAttribute::Constexpr;
      } else {
        return Cxx::LocalVariableAttribute::Plain;
      }
    }

    static folly::Optional<VarDecl> declare(
        ASTVisitor& visitor,
        const clang::VarDecl *decl,
        Cxx::Scope scope,
        Src::Range range) {
      return folly::variant_match<folly::Optional<VarDecl>>(
        variableKind(decl),
        [](std::monostate) {
          return folly::none;
        },
        [&](Cxx::GlobalVariableKind kind) {
          auto qname = visitor.db.fact<Cxx::QName>(
            visitor.db.name(decl->getName()), scope);

          return VarDecl
              { {}
              , qname
              , visitor.db.fact<Cxx::VariableDeclaration>(
                  qname,
                  visitor.type(decl->getType()),
                  Cxx::VariableKind::global_(
                    Cxx::GlobalVariable{
                      kind,
                      globalAttribute(decl),
                      decl->isThisDeclarationADefinition()
                        == clang::VarDecl::Definition
                  }),
                  range)
              };
        },
        [&](Cxx::LocalVariableKind kind) {
          auto qname = visitor.db.fact<Cxx::QName>(
            visitor.db.name(decl->getName()), scope);

          return VarDecl
              { {}
              , qname
              , visitor.db.fact<Cxx::VariableDeclaration>(
                  qname,
                  visitor.type(decl->getType()),
                  Cxx::VariableKind::local(
                    Cxx::LocalVariable{
                      kind,
                      localAttribute(decl),
                  }),
                  range)
              };
        }
      );
    }

    static Cxx::VariableKind fieldKind(
        ASTVisitor& visitor,
        const clang::FieldDecl *decl) {
      folly::Optional<uint64_t> bitsize;
      if (auto size_expr = decl->getBitWidth()) {
        // Consider the following code:
        //
        // template<class T> class U { unsigned i : sizeof(T); };
        //
        // Here, i is a bit field but it doesn't have a fixed bit size. In fact,
        // Clang segfaults if we call getBitWidthValue on it. So let's give it
        // size 0 for now - we should probably extend the schema eventually.
        bitsize = size_expr->isValueDependent()
          ? 0
          : decl->getBitWidthValue(visitor.astContext);
      }
      if (auto ivar = clang::dyn_cast<clang::ObjCIvarDecl>(decl)) {
        return Cxx::VariableKind::ivar(
          Cxx::ObjcIVar{ivar->getSynthesize(), maybe(bitsize)}
        );
      } else {
        return Cxx::VariableKind::field(
          Cxx::Field{decl->isMutable(), maybe(bitsize)}
        );
      }
    }

    static folly::Optional<VarDecl> declare(
        ASTVisitor& visitor,
        const clang::FieldDecl *decl,
        Cxx::Scope scope,
        Src::Range range) {
      auto qname = visitor.db.fact<Cxx::QName>(
        visitor.db.name(decl->getName()), scope);

      return VarDecl
        { {}
        , qname
        , visitor.db.fact<Cxx::VariableDeclaration>(
            qname,
            visitor.type(decl->getType()),
            fieldKind(visitor, decl),
            range)
        };
    }

    void define(ASTVisitor&, const clang::VarDecl *) const {}
    void define(ASTVisitor&, const clang::FieldDecl *) const {}

    struct GetTemplatableDecl {
      const clang::VarDecl* FOLLY_NULLABLE
      operator()(const clang::VarDecl* decl) const {
        // The logic for `VarDecl` is a bit simpler here due to LLVM handling
        // the cases we handle manually for `CXXRecordDecl` and `FunctionDecl`.
        // https://github.com/llvm/llvm-project/blob/a3a2239aaaf6860eaee591c70a016b7c5984edde/clang/lib/AST/Decl.cpp#L2608-L2619
        return decl->getTemplateInstantiationPattern();
      }

      const clang::FieldDecl* FOLLY_NULLABLE
      operator()(const clang::FieldDecl* decl) const {
        return DeclTraits::getTemplateInstantiationPattern(decl);
      }
    };

    static const clang::VarDecl * FOLLY_NULLABLE getSpecializedDecl(
        const clang::VarDecl *decl) {
      if (auto spec =
              clang::dyn_cast<clang::VarTemplateSpecializationDecl>(decl)) {
        auto tpl = spec->getSpecializedTemplate();
        return tpl ? tpl->getTemplatedDecl() : nullptr;
      }
      return nullptr;
    }
  };

  bool VisitVarDecl(const clang::VarDecl *decl) {
    // We check for implicitly instantiated variable declarations here due to a
    // bug in `RecursiveASTVisitor`. With `shouldVisitTemplateInstantions` set
    // to return `false`, `RecursiveASTVisitor` is supposed to skip all implicit
    // template instantiations. For variable templates and out-of-line
    // definition of static variables, nodes are injected into the enclosing
    // `DeclContext`. For example:
    //
    //   namespace N {
    //     template <typename T> bool value;
    //   }
    //
    //   void h() {
    //     (void)N::value<int>;
    //     (void)N::value<double>;
    //   }
    //
    // There is a `VarTemplateDecl` with 2 `VarTemplateSpecializationDecl`s
    // nested inside it, which `RecursiveASTVisitor` skips correctly.
    // But for some reason there are 2 `VarTemplateSpecializationDecl`s directly
    // under `N` as well. `RecursiveASTVisitor` fails to skip these correctly.
    // As such, we skip them manually here.
    if (clang::isTemplateInstantiation(decl->getTemplateSpecializationKind())) {
      return true;
    }
    visitDeclaration(varDecls, decl);
    return true;
  }

  bool VisitFieldDecl(const clang::FieldDecl *decl) {
    visitDeclaration(varDecls, decl);
    return true;
  }


  /*******************
   * ObjC containers *
   *******************/

  std::vector<Fact<Cxx::ObjcContainerDeclaration>> objcContainerProtocols(
      const clang::ObjCContainerDecl *decl) {
    const clang::ObjCProtocolList *list = nullptr;
    if (auto prot = clang::dyn_cast<clang::ObjCProtocolDecl>(decl)) {
      list = &prot->getReferencedProtocols();
    } else if (auto iface = clang::dyn_cast<clang::ObjCInterfaceDecl>(decl)) {
      list = &iface->getReferencedProtocols();
    } else if (auto cat = clang::dyn_cast<clang::ObjCCategoryDecl>(decl)) {
      list = &cat->getReferencedProtocols();
    }

    std::vector<Fact<Cxx::ObjcContainerDeclaration>> protocols;
    if (list) {
      if (auto loc = list->loc_begin()) {
        for (auto prot : *list) {
          // NOTE: Protocols don't seem to be visited anywhere so record xrefs
          // here.
          xrefObjCProtocolDecl(*loc, prot);
          ++loc;
          if (auto p = objcContainerDecls(prot)) {
            protocols.push_back(p->decl);
          }
        }
      }
    }

    return protocols;
  }

  Fact<Cxx::Declarations> objcContainerMembers(
      const clang::ObjCContainerDecl *decl) {
    std::vector<Cxx::Declaration> members;

    for (auto member : decl->decls()) {
      if (auto method = clang::dyn_cast<clang::ObjCMethodDecl>(member)) {
        if (auto d = objcMethodDecls(method)) {
          members.push_back(Cxx::Declaration::objcMethod(d->decl));
        }
      }
      if (auto property = clang::dyn_cast<clang::ObjCPropertyDecl>(member)) {
        if (auto d = objcPropertyDecls(property)) {
          members.push_back(Cxx::Declaration::objcProperty(d->decl));
        }
      }
      if (auto ivar = clang::dyn_cast<clang::ObjCIvarDecl>(member)) {
        if (auto d = varDecls(ivar)) {
          members.push_back(Cxx::Declaration::variable(d->decl));
        }
      }
    }
    return db.fact<Cxx::Declarations>(std::move(members));
    }

  struct ObjcContainerDecl : Declare<ObjcContainerDecl> {
    Cxx::ObjcContainerId id;
    Fact<Cxx::ObjcContainerDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::objcContainer(decl);
    }

    template<typename Decl, typename... Decls, typename ClangDecl>
    static std::optional<ObjcContainerDecl> findIn(
        ASTVisitor& visitor,
        const ClangDecl *decl) {
      if (auto p = clang::dyn_cast<Decl>(decl)) {
        return visitor.objcContainerDecls(p);
      } else {
        return findIn<Decls...>(visitor, decl);
      }
    }

    template<typename ClangDecl>
    static std::optional<ObjcContainerDecl> findIn(
        ASTVisitor&,
        const ClangDecl *) {
      return std::nullopt;
    }

    template<typename ClangDecl>
    static std::optional<ObjcContainerDecl> find(
        ASTVisitor& visitor,
        const ClangDecl * FOLLY_NULLABLE decl) {
      if (decl) {
        return findIn<
                clang::ObjCProtocolDecl,
                clang::ObjCInterfaceDecl,
                clang::ObjCCategoryDecl,
                clang::ObjCImplementationDecl,
                clang::ObjCCategoryImplDecl>(visitor, decl);
        } else {
          return std::nullopt;
        }
    }

    template<typename Decl>
    static std::optional<Cxx::ObjcCategoryId> categoryId(
        ASTVisitor& visitor,
        const Decl *decl) {
      if (auto iface = decl->getClassInterface()) {
        return Cxx::ObjcCategoryId{
          visitor.db.name(iface->getName()),
          visitor.db.name(decl->getName())
        };
      } else {
        return std::nullopt;
      }
    }

    static folly::Optional<Cxx::ObjcContainerId> containerId(
        ASTVisitor& visitor,
        const clang::ObjCProtocolDecl *decl) {
      return Cxx::ObjcContainerId::protocol(visitor.db.name(decl->getName()));
    }

    static folly::Optional<Cxx::ObjcContainerId> containerId(
        ASTVisitor& visitor,
        const clang::ObjCInterfaceDecl *decl) {
      return Cxx::ObjcContainerId::interface_(
        visitor.db.name(decl->getName()));
    }

    static folly::Optional<Cxx::ObjcContainerId> containerId(
        ASTVisitor& visitor,
        const clang::ObjCCategoryDecl *decl) {
      if (decl->IsClassExtension()) {
        if (auto iface = decl->getClassInterface()) {
          return Cxx::ObjcContainerId::extensionInterface(
            visitor.db.name(iface->getName()));
        } else {
          return folly::none;
        }
      } else if (auto id = categoryId(visitor, decl)) {
        return Cxx::ObjcContainerId::categoryInterface(id.value());
      } else {
        return folly::none;
      }
    }

    static folly::Optional<Cxx::ObjcContainerId> containerId(
        ASTVisitor& visitor,
        const clang::ObjCImplementationDecl *decl) {
      if (auto iface = decl->getClassInterface()) {
        return Cxx::ObjcContainerId::implementation(
          visitor.db.name(iface->getName()));
      } else {
        return folly::none;
      }
    }

    static folly::Optional<Cxx::ObjcContainerId> containerId(
        ASTVisitor& visitor,
        const clang::ObjCCategoryImplDecl *decl) {
      if (auto id = categoryId(visitor, decl)) {
        return Cxx::ObjcContainerId::categoryImplementation(id.value());
      } else {
        return folly::none;
      }
    }

    template<typename Decl>
    static folly::Optional<ObjcContainerDecl> declare(
        ASTVisitor& visitor,
        const Decl *decl,
        Cxx::Scope,
        Src::Range range) {
      if (folly::Optional<Cxx::ObjcContainerId> id
            = ObjcContainerDecl::containerId(visitor, decl)) {
        return ObjcContainerDecl
          { {}
          , id.value()
          , visitor.db.fact<Cxx::ObjcContainerDeclaration>(id.value(), range)
          };
      } else {
        return folly::none;
      }
    }

    static std::vector<ObjcContainerDecl> implements(
        ASTVisitor&,
        const clang::ObjCProtocolDecl *) {
      return {};
    }

    static std::vector<ObjcContainerDecl> implements(
        ASTVisitor&,
        const clang::ObjCInterfaceDecl *) {
      return {};
    }

    static std::vector<ObjcContainerDecl> implements(
        ASTVisitor&,
        const clang::ObjCCategoryDecl *) {
      return {};
    }

    static std::vector<ObjcContainerDecl> implements(
        ASTVisitor& visitor,
        const clang::ObjCImplementationDecl *decl) {
      std::vector<ObjcContainerDecl> decls;
      if (auto iface = decl->getClassInterface()) {
        if (auto r = visitor.objcContainerDecls(iface)) {
          decls.push_back(r.value());
        }
        for (auto cat : iface->known_extensions()) {
          if (auto r = visitor.objcContainerDecls(cat)) {
            decls.push_back(r.value());
          }
        }
      }
      return decls;
    }

    static std::vector<ObjcContainerDecl> implements(
        ASTVisitor& visitor,
        const clang::ObjCCategoryImplDecl *decl) {
      std::vector<ObjcContainerDecl> decls;
        if (auto cat = decl->getCategoryDecl()) {
        if (auto r = visitor.objcContainerDecls(cat)) {
          decls.push_back(r.value());
        }
      }
      return decls;
    }

    static folly::Optional<ObjcContainerDecl> base(
        ASTVisitor&,
        const clang::ObjCImplementationDecl *) {
      return folly::none;
    }

    static folly::Optional<ObjcContainerDecl> base(
        ASTVisitor&,
        const clang::ObjCCategoryDecl*) {
      return folly::none;
    }

    static folly::Optional<ObjcContainerDecl> base(
        ASTVisitor&,
        const clang::ObjCProtocolDecl*) {
      return folly::none;
    }

    static folly::Optional<ObjcContainerDecl> base(
        ASTVisitor& visitor,
        const clang::ObjCInterfaceDecl* decl) {
      if (auto sclass = decl->getSuperClass()) {
        if (auto r = visitor.objcContainerDecls(sclass)) {
          return r.value();
        }
      }
      return folly::none;
    }

    static folly::Optional<ObjcContainerDecl> base(
        ASTVisitor&,
        const clang::ObjCCategoryImplDecl*) {
      return folly::none;
    }

    template<typename Decl>
    void define(ASTVisitor& visitor, const Decl *d) {
      visitor.db.fact<Cxx::ObjcContainerDefinition>(
        decl,
        visitor.objcContainerProtocols(d),
        visitor.objcContainerMembers(d));

      auto xs = implements(visitor, d);
      for (const auto& x : xs) {
        visitor.db.fact<Cxx::ObjcImplements>(decl, x.decl);
      }

      if(auto bs = base(visitor, d)) {
        visitor.db.fact<Cxx::ObjcContainerBase>(decl, bs->decl);
      }
    }
  };

  bool VisitObjCProtocolDecl(const clang::ObjCProtocolDecl *decl) {
    visitDeclaration(objcContainerDecls, decl);
    return true;
  }

  bool VisitObjCInterfaceDecl(const clang::ObjCInterfaceDecl *decl) {
    visitDeclaration(objcContainerDecls, decl);
    return true;
  }

  bool VisitObjCCategoryDecl(const clang::ObjCCategoryDecl *decl) {
    visitDeclaration(objcContainerDecls, decl);
    return true;
  }

  bool VisitObjCImplementationDecl(const clang::ObjCImplementationDecl *decl) {
    visitDeclaration(objcContainerDecls, decl);
    return true;
  }

  bool VisitObjCCategoryImplDecl(const clang::ObjCCategoryImplDecl *decl) {
    visitDeclaration(objcContainerDecls, decl);
    return true;
  }

  /****************
   * ObjC methods *
   ****************/

  std::pair<Fact<Cxx::ObjcSelector>, std::vector<std::string>> objcSelector(
      const clang::Selector& sel) {
    std::vector<std::string> sels;
    if (sel.isUnarySelector()) {
      // It seems that for unary selectors (i.e., selectors which are just a
      // name with no arguments), getNameForSlot(0) returns "".
      sels.push_back(sel.getAsString());
    } else {
      const size_t n = sel.getNumArgs();
      sels.reserve(n);
      for (size_t i = 0; i < n; ++i) {
        sels.push_back(sel.getNameForSlot(i).str());
      }
    }
    return {db.fact<Cxx::ObjcSelector>(sels), std::move(sels)};
  }

  std::vector<Src::FileLocation> objcSelectorLocations(
      const clang::ObjCMethodDecl* d) {
    const auto n = d->getNumSelectorLocs();
    std::vector<Src::FileLocation> spans;
    spans.reserve(n);
    for (unsigned int i = 0; i < n; ++i) {
      auto range = folly::variant_match(
          db.fullSrcRange(d->getSelectorLoc(i)),
          [](const ClangDB::SourceRange& range) { return range; },
          [](const auto& range) {
            const auto& [expansion, spelling] = range;
            return spelling && spelling->file ? *spelling : expansion;
          });
      spans.push_back({range.file->fact, range.span});
    }
    return spans;
  }

  struct ObjcMethodDecl : Declare<ObjcMethodDecl> {
    Fact<Cxx::ObjcMethodDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::objcMethod(decl);
    }

    static folly::Optional<ObjcMethodDecl> declare(
        ASTVisitor& visitor,
        const clang::ObjCMethodDecl *d,
        Cxx::Scope,
        Src::Range range) {
      if (auto container =
            ObjcContainerDecl::find(visitor, d->getDeclContext())) {
        auto fact = visitor.db.fact<Cxx::ObjcMethodDeclaration>(
            visitor.objcSelector(d->getSelector()).first,
            visitor.objcSelectorLocations(d),
            container->id,
            visitor.signature(d->getReturnType(), d->parameters()),
            d->isInstanceMethod(),
            d->isOptional(),
            d->isPropertyAccessor(),
            range);
        visitor.db.fact<Cxx::ObjcMethodDeclarationName>(
            fact,
            // NOTE: This use of `getNameAsString` is intentional.
            visitor.db.name(d->getNameAsString()));
        return ObjcMethodDecl{{}, fact};
      } else {
        return folly::none;
      }
    }

    void define(ASTVisitor& visitor, const clang::ObjCMethodDecl *) const {
      visitor.db.fact<Cxx::ObjcMethodDefinition>(decl);
    }
  };

  bool VisitObjCMethodDecl(const clang::ObjCMethodDecl *decl) {
    visitDeclaration(objcMethodDecls, decl);
    return true;
  }

  /*******************
   * ObjC properties *
   *******************/

  struct ObjcPropertyDecl : Declare<ObjcPropertyDecl> {
    Fact<Cxx::ObjcPropertyDeclaration> decl;

    Cxx::Declaration declaration() const {
      return Cxx::Declaration::objcProperty(decl);
    }

    static folly::Optional<ObjcPropertyDecl> declare(
        ASTVisitor& visitor,
        const clang::ObjCPropertyDecl *d,
        Cxx::Scope,
        Src::Range range) {
      if (auto container =
            ObjcContainerDecl::find(visitor, d->getDeclContext())) {
        return ObjcPropertyDecl
          { {}
          , visitor.db.fact<Cxx::ObjcPropertyDeclaration>(
              visitor.db.name(d->getName()),
              container->id,
              visitor.type(d->getType()),
              d->isInstanceProperty(),
              d->isOptional(),
              d->isReadOnly(),
              d->isAtomic(),
              range)
          };
      } else {
        return folly::none;
      }
    }

    void define(ASTVisitor&, const clang::ObjCPropertyDecl *) const {
      // TODO: complete
    }
  };

  bool VisitObjCPropertyDecl(const clang::ObjCPropertyDecl *decl) {
    visitDeclaration(objcPropertyDecls, decl);
    return true;
  }

  bool VisitObjCPropertyImplDecl(const clang::ObjCPropertyImplDecl *decl) {
    if (decl->getSourceRange().isValid()) {
      if (auto r = objcPropertyDecls(decl->getPropertyDecl())) {
        // TODO: remove?
        folly::Optional<Fact<Cxx::Name>> name;
        if (decl->isIvarNameSpecified()) {
          name = db.name(decl->getPropertyIvarDecl()->getName());
        }

        if (auto ivar = decl->getPropertyIvarDecl()) {
          if (auto s = varDecls(ivar)) {
            db.fact<Cxx::ObjcPropertyIVar>(r->decl, s->decl);
          }
        }

        db.fact<Cxx::ObjcPropertyImplementation>(
          r->decl,
          decl->getPropertyImplementation()
            == clang::ObjCPropertyImplDecl::Synthesize
            ? Cxx::ObjcPropertyKind::Synthesize
            : Cxx::ObjcPropertyKind::Dynamic,
          maybe(name),
          db.srcRange(decl->getSourceRange()).range);
      }
    }
    return true;
  }

  struct XRef {
    const clang::Decl *primary;
    const clang::Decl *decl;
    folly::Optional<Cxx::Declaration> gleanDecl;
    folly::Optional<Cxx::XRefTarget> target;

    static XRef unknown(const clang::Decl* d) {
      return {d, d, folly::none, folly::none};
    }

    static XRef to(const clang::Decl* d, Cxx::XRefTarget t) {
      return {d, d, folly::none, std::move(t)};
    }

    template <typename Memo, typename Decl>
    static XRef toDecl(Memo& memo, const Decl* decl) {
      auto b = memo(decl);
      auto d = b ? b->key : decl;
      if (b) {
        return {
            d,
            d,
            representative(memo,d,*b).declaration(),
            Cxx::XRefTarget::declaration(b->declaration())};
      } else {
        return unknown(d);
      }
    }
  };

  void xrefTarget(clang::SourceRange range, XRef xref) {
    if (DeclTraits::isImplicit(xref.decl)) {
      return;
    }
    const auto raw = xref.target
      ? xref.target.value()
      : unknownTarget(xref.decl);
    const auto wrapped = usingTracker.retarget(xref.primary, raw);
    auto sort_id = db.srcRange(xref.decl->getLocation());
    db.xref(range, wrapped, sort_id, raw == wrapped);
    if (xref.gleanDecl && !clang::isa<clang::NamespaceDecl>(xref.decl)) {
      declTargets.contextXRef(*xref.gleanDecl, sort_id);
    }
  }

  Cxx::XRefTarget unknownTarget(const clang::Decl* decl) {
    return Cxx::XRefTarget::unknown(db.srcLoc(decl->getBeginLoc()));
  }

  void xrefDecl(
      clang::SourceRange range,
      const clang::Decl* FOLLY_NULLABLE decl) {
    struct XRefVisitor : clang::ConstDeclVisitor<XRefVisitor, XRef> {
      explicit XRefVisitor(ASTVisitor& v) : visitor_(v) {}
      XRef VisitDecl(const clang::Decl* d) {
        return XRef::unknown(d);
      }
      XRef VisitCXXRecordDecl(const clang::CXXRecordDecl* d) {
        return XRef::toDecl(visitor_.classDecls, d);
      }
      XRef VisitClassTemplateSpecializationDecl(
          const clang::ClassTemplateSpecializationDecl* d) {
        auto xref = XRef::toDecl(visitor_.classDecls, d);
        if (const auto* tpl = d->getSpecializedTemplate()) {
          if (const auto* primary = tpl->getTemplatedDecl()) {
            xref.primary = primary;
          }
        }
        return xref;
      }
      XRef VisitEnumDecl(const clang::EnumDecl* d) {
        return XRef::toDecl(visitor_.enumDecls, d);
      }
      XRef VisitEnumConstantDecl(const clang::EnumConstantDecl* d) {
        auto e = visitor_.enumeratorDecls(d);
        CHECK(e);
        return XRef::to(d, Cxx::XRefTarget::enumerator(e->fact));
      }
      XRef VisitFieldDecl(const clang::FieldDecl* d) {
        return XRef::toDecl(visitor_.varDecls, d);
      }
      XRef VisitFunctionDecl(const clang::FunctionDecl* d) {
        auto xref = XRef::toDecl(visitor_.funDecls, d);
        if (const auto* tpl = d->getPrimaryTemplate()) {
          if (const auto* primary = tpl->getTemplatedDecl()) {
            xref.primary = primary;
          }
        }
        return xref;
      }
      XRef VisitNamespaceDecl(const clang::NamespaceDecl* d) {
        return XRef::toDecl(visitor_.namespaces, d);
      }
      XRef VisitObjCCategoryDecl(const clang::ObjCCategoryDecl* d) {
        return XRef::toDecl(visitor_.objcContainerDecls, d);
      }
      XRef VisitObjCCategoryImplDecl(const clang::ObjCCategoryImplDecl* d) {
        return XRef::toDecl(visitor_.objcContainerDecls, d);
      }
      XRef VisitObjCImplementationDecl(const clang::ObjCImplementationDecl* d) {
        return XRef::toDecl(visitor_.objcContainerDecls, d);
      }
      XRef VisitObjCInterfaceDecl(const clang::ObjCInterfaceDecl* d) {
        return XRef::toDecl(visitor_.objcContainerDecls, d);
      }
      XRef VisitObjCProtocolDecl(const clang::ObjCProtocolDecl* d) {
        return XRef::toDecl(visitor_.objcContainerDecls, d);
      }
      XRef VisitObjCMethodDecl(const clang::ObjCMethodDecl* d) {
        return XRef::toDecl(visitor_.objcMethodDecls, d);
      }
      XRef VisitObjCPropertyDecl(const clang::ObjCPropertyDecl* d) {
        return XRef::toDecl(visitor_.objcPropertyDecls, d);
      }
      XRef VisitRedeclarableTemplateDecl(
          const clang::RedeclarableTemplateDecl* d) {
        return Visit(d->getTemplatedDecl());
      }
      XRef VisitTypedefNameDecl(const clang::TypedefNameDecl* d) {
        return XRef::toDecl(visitor_.typeAliasDecls, d);
      }
      XRef VisitNamespaceAliasDecl(const clang::NamespaceAliasDecl* d) {
        return XRef::toDecl(visitor_.namespaceAliasDecls, d);
      }
      XRef VisitUsingShadowDecl(const clang::UsingShadowDecl* d) {
        return Visit(d->getTargetDecl());
      }
      XRef VisitVarDecl(const clang::VarDecl* d) {
        return XRef::toDecl(visitor_.varDecls, d);
      }
      XRef VisitVarTemplateSpecializationDecl(
          const clang::VarTemplateSpecializationDecl* d) {
        auto xref = XRef::toDecl(visitor_.varDecls, d);
        if (const auto* tpl = d->getSpecializedTemplate()) {
          if (const auto* primary = tpl->getTemplatedDecl()) {
            xref.primary = primary;
          }
        }
        return xref;
      }
      ASTVisitor& visitor_;
    };
    if (decl) {
      xrefTarget(range, XRefVisitor(*this).Visit(decl));
    }
  }

  void xrefObjCProtocolDecl(
      clang::SourceLocation loc,
      const clang::ObjCProtocolDecl *decl) {
    if (loc.isValid()) {
      xrefDecl(loc, decl);
    }
  }

  /*****************
   * Type visitors *
   *****************/

  bool VisitTagTypeLoc(clang::TagTypeLoc tloc) {
    xrefDecl(tloc.getSourceRange(), tloc.getDecl());
    return true;
  }

  bool VisitTypedefTypeLoc(clang::TypedefTypeLoc tloc) {
    xrefDecl(tloc.getSourceRange(), tloc.getTypedefNameDecl());
    return true;
  }

#if LLVM_VERSION_MAJOR >= 14
  bool VisitUsingTypeLoc(clang::UsingTypeLoc tloc) {
    xrefDecl(tloc.getSourceRange(), tloc.getFoundDecl());
    return true;
  }
#endif

  void xrefTemplateName(clang::SourceLocation loc, clang::TemplateName name) {
      xrefDecl(loc, name.getAsTemplateDecl());
  }

  bool VisitTemplateSpecializationTypeLoc(
      clang::TemplateSpecializationTypeLoc tloc) {
    xrefTemplateName(
      tloc.getTemplateNameLoc(),
      tloc.getTypePtr()->getTemplateName());
    return true;
  }

  bool VisitObjCObjectTypeLoc(clang::ObjCObjectTypeLoc tloc) {
    const auto n = tloc.getNumProtocols();
    const auto locs = tloc.getProtocolLocs();
    for (size_t i = 0; i < n; ++i) {
      xrefObjCProtocolDecl(locs[i], tloc.getProtocol(i));
    }
    return true;
  }

  bool VisitObjCInterfaceTypeLoc(clang::ObjCInterfaceTypeLoc tloc) {
    xrefDecl(tloc.getLocalSourceRange(), tloc.getIFaceDecl());
    return true;
  }

  bool TraverseElaboratedTypeLoc(clang::ElaboratedTypeLoc tloc) {
    return usingTracker.inNameContext(tloc.getTypePtr()->getQualifier(),
      [&] { return Base::TraverseElaboratedTypeLoc(tloc); });
  }

  bool TraverseTemplateArgumentLoc(const clang::TemplateArgumentLoc& arg) {
    // X::T<U> is an ElaboratedType and so we traverse the T<U> bit in the
    // context of X. This is correct for T but wrong for U so we have to reset
    // the context when traversing template arguments.
    return usingTracker.inNameContext(nullptr, [&] {
      switch (arg.getArgument().getKind()) {
        case clang::TemplateArgument::Template:
        case clang::TemplateArgument::TemplateExpansion:
          // This handles cases like:
          //
          // template<typename T> A {};
          // template<template<typename> typename T> B {};
          //
          // B<A> x;
          //
          // The template argument A isn't a proper type here and hence isn't
          // traversed by the usual machinery as such. It is a TemplateName but
          // those don't have source locations so we have to do it in the parent
          // nodes.
          usingTracker.inNameContext(
            arg.getTemplateQualifierLoc().getNestedNameSpecifier(),
            [&] { xrefTemplateName(
              arg.getTemplateNameLoc(),
              arg.getArgument().getAsTemplateOrTemplatePattern()); });
          break;

        default:
          break;
      }
      return Base::TraverseTemplateArgumentLoc(arg); });
  }

  // TODO: It's not clear if/when this is used instead of
  // TraverseTemplateArgumentLoc
  bool TraverseTemplateArguments(
#if LLVM_VERSION_MAJOR >= 17 || \
    (LLVM_VERSION_MAJOR >= 16 && defined(CAST_TARGET_REPO_FBANDROID))
      llvm::ArrayRef<clang::TemplateArgument> args
#else
      const clang::TemplateArgument* args,
      unsigned num
#endif
  ) {
    return usingTracker.inNameContext(nullptr,
      [&] {
#if LLVM_VERSION_MAJOR >= 17 || \
    (LLVM_VERSION_MAJOR >= 16 && defined(CAST_TARGET_REPO_FBANDROID))
        return Base::TraverseTemplateArguments(args);
#else
        return Base::TraverseTemplateArguments(args, num);
#endif
        });
  }

  bool TraverseDecl(clang::Decl *decl) {
    clang::DeclContext *context = nullptr;

    folly::Optional<Cxx::Declaration> gleanDecl;
    auto traverse = [&]() {
      if (gleanDecl) {
        return declTargets.inDeclaration(*gleanDecl, [&] {
          return Base::TraverseDecl(decl); });
      } else {
        return Base::TraverseDecl(decl);
      }
    };

    if (decl) {
      if (auto tunit = clang::dyn_cast<clang::TranslationUnitDecl>(decl)) {
        context = tunit;
      } else if (auto ns = clang::dyn_cast<clang::NamespaceDecl>(decl)) {
        // FIXME: This isn't quite right, the qualified name should be
        // traversed in the enclosing context.
        context = ns;
      } else if (auto tag = clang::dyn_cast<clang::TagDecl>(decl)) {
        // FIXME: This isn't quite right, the qualified name should be
        // traversed in the enclosing context. Not sure about base classes.
        context = tag;
      } else if (auto fun = clang::dyn_cast<clang::FunctionDecl>(decl)) {
        if (auto f = funDecls(fun)) {
          gleanDecl = f->declaration();
        }
        return usingTracker.inFunction(fun, traverse);
      }

      struct GetGleanDecl : clang::DeclVisitor<
                               GetGleanDecl,
                               folly::Optional<Cxx::Declaration>> {
        ASTVisitor& visitor_;

        explicit GetGleanDecl(ASTVisitor& v) : visitor_(v) {}
        folly::Optional<Cxx::Declaration> VisitDecl(clang::Decl *d) {
          return folly::none;
        }
        folly::Optional<Cxx::Declaration> VisitCXXRecordDecl(
            clang::CXXRecordDecl* d) {
          if (auto g = visitor_.classDecls(d)) {
            return g->declaration();
          } else {
            return folly::none;
          }
        }
        folly::Optional<Cxx::Declaration> VisitObjCCategoryDecl(
            clang::ObjCCategoryDecl* d) {
          if (auto g = visitor_.objcContainerDecls(d)) {
            return g->declaration();
          } else {
            return folly::none;
          }
        }
        folly::Optional<Cxx::Declaration> VisitObjCCategoryImplDecl(
            clang::ObjCCategoryImplDecl* d) {
          if (auto g = visitor_.objcContainerDecls(d)) {
            return g->declaration();
          } else {
            return folly::none;
          }
        }
        folly::Optional<Cxx::Declaration> VisitObjCImplementationDecl(
            clang::ObjCImplementationDecl* d) {
          if (auto g = visitor_.objcContainerDecls(d)) {
            return g->declaration();
          } else {
            return folly::none;
          }
        }
        folly::Optional<Cxx::Declaration> VisitObjCInterfaceDecl(
            clang::ObjCInterfaceDecl* d) {
          if (auto g = visitor_.objcContainerDecls(d)) {
            return g->declaration();
          } else {
            return folly::none;
          }
        }
        folly::Optional<Cxx::Declaration> VisitObjCProtocolDecl(
            clang::ObjCProtocolDecl* d) {
          if (auto g = visitor_.objcContainerDecls(d)) {
            return g->declaration();
          } else {
            return folly::none;
          }
        }
        folly::Optional<Cxx::Declaration> VisitObjCMethodDecl(
            clang::ObjCMethodDecl* d) {
          if (auto g = visitor_.objcMethodDecls(d)) {
            return g->declaration();
          } else {
            return folly::none;
          }
        }
        folly::Optional<Cxx::Declaration> VisitObjCPropertyDecl(
            clang::ObjCPropertyDecl* d) {
          if (auto g = visitor_.objcPropertyDecls(d)) {
            return g->declaration();
          } else {
            return folly::none;
          }
        }
      };
      gleanDecl = GetGleanDecl(*this).Visit(decl);
    }
    return usingTracker.inContext(context, traverse);
  }

  bool TraverseCompoundStmt(clang::CompoundStmt *stmt) {
    return usingTracker.maybeBody(
      stmt,
      [&] { return Base::TraverseCompoundStmt(stmt) ; });
  }

  bool TraverseConstructorInitializer(clang::CXXCtorInitializer* init) {
    xrefDecl(init->getMemberLocation(), init->getMember());
    return Base::TraverseConstructorInitializer(init);
  }

  bool TraverseCoroutineBodyStmt(clang::CoroutineBodyStmt *stmt) {
    return usingTracker.maybeBody(
      stmt,
      [&] { return Base::TraverseCoroutineBodyStmt(stmt); });
  }

  // This helper is used to traverse the implicit and explicit components of
  // a subtree. For example, given a coroutine coreturn statement:
  //
  //   Task<int> f() {
  //     int x = 42;
  //     co_return x;
  //     //        ^ explicit xref to `x`.
  //     // implicit xref to `Task<int>::promise_type::return_value(x)`.
  //   }
  //
  // The explicit AST traversal (default) visits the xref to the local `x`.
  // The implicit AST traversal (when `shouldVisitImplicitCode()` returns true)
  // visits the hidden xref to `Task<int>::promise_type::return_value(x);`.
  //
  // We save and set the state of `shouldVisitImplicitCode_` to control the
  // result of `shouldVisitImplicitCode()` used by the Clang traversal logic.
  template <typename F>
  bool traverseImplicitAndExplicitCode(F&& f) {
    auto saved = shouldVisitImplicitCode_;
    SCOPE_EXIT {
      shouldVisitImplicitCode_ = saved;
    };
    for (bool b : {false, true}) {
      shouldVisitImplicitCode_ = b;
      if (!std::forward<F>(f)()) {
        return false;
      }
    }
    return true;
  }

  bool TraverseCoreturnStmt(clang::CoreturnStmt* stmt) {
    return traverseImplicitAndExplicitCode([&] {
      return Base::TraverseCoreturnStmt(stmt);
    });
  }

  bool TraverseCoawaitExpr(clang::CoawaitExpr* expr) {
    return traverseImplicitAndExplicitCode([&] {
      return Base::TraverseCoawaitExpr(expr);
    });
  }

  bool TraverseDependentCoawaitExpr(clang::DependentCoawaitExpr* expr) {
    return traverseImplicitAndExplicitCode([&] {
      return Base::TraverseDependentCoawaitExpr(expr);
    });
  }

  bool TraverseCoyieldExpr(clang::CoyieldExpr* expr) {
    return traverseImplicitAndExplicitCode([&] {
      return Base::TraverseCoyieldExpr(expr);
    });
  }

  bool TraverseLambdaExpr(clang::LambdaExpr *lambda) {
    return usingTracker.inFunction(
      lambda->getCallOperator(),
      [&] { return Base::TraverseLambdaExpr(lambda); });
  }

  bool TraverseParmVarDecl(clang::ParmVarDecl *decl) {
    // Thankfully, ParmVarDecls seem to have the right context
    if (auto *context = decl->getDeclContext()) {
      if (clang::isa<clang::FunctionDecl>(context)) {
        return usingTracker.inContext(
          context,
          [&] { return Base::TraverseParmVarDecl(decl); });
      }
    }
    return Base::TraverseParmVarDecl(decl);
  }

  // TODO: set the right context for constructor initialisers and exception
  // specs


  // NOTE: For some reason, RecursiveASTVisitor doesn't seem to call
  // VisitNestedNameSpecifierLoc
  bool TraverseNestedNameSpecifierLoc(clang::NestedNameSpecifierLoc loc) {
    auto spec = loc.getNestedNameSpecifier();
    return usingTracker.inNameContext(spec ? spec->getPrefix() : nullptr,
      [&] {
        if (spec) {
          if (auto ns = spec->getAsNamespace()) {
            xrefDecl(loc.getLocalBeginLoc(), ns->getCanonicalDecl());
          } else if (auto ns = spec->getAsNamespaceAlias()) {
            xrefDecl(loc.getLocalBeginLoc(), ns->getCanonicalDecl());
          }
        }
        return Base::TraverseNestedNameSpecifierLoc(loc);
      });
  }

  void xrefExpr(
      clang::SourceRange range,
      const clang::NestedNameSpecifier* FOLLY_NULLABLE qualifier,
      const clang::Decl* FOLLY_NULLABLE decl) {
    if (decl) {
      usingTracker.inNameContext(qualifier, [&] { xrefDecl(range, decl); });
    }
  }

  bool VisitCXXConstructExpr(const clang::CXXConstructExpr* expr) {
    xrefDecl(expr->getSourceRange(), expr->getConstructor());
    return true;
  }

  bool VisitDeclRefExpr(const clang::DeclRefExpr* expr) {
    xrefExpr(
        expr->getNameInfo().getSourceRange(),
        expr->getQualifier(),
        expr->getDecl());
    return true;
  }

  bool VisitOverloadExpr(const clang::OverloadExpr *expr) {
    const auto qualifier = expr->getQualifier();
    const auto range = expr->getNameInfo().getSourceRange();
    // For now, we xref all functions in the overload set:
    //
    // void f(int); // A
    // void f(bool); // B
    //
    // template<class T> void g(T x) { f(x); } // will xref both A and B
    //
    // This is especially important for using decls.
    for (const auto *decl : expr->decls()) {
      xrefExpr(range, qualifier, decl);
    }
    return true;
  }

  bool VisitMemberExpr(const clang::MemberExpr *expr) {
    // getMemberNameInfo().getSourceRange() can return an invalid range if,
    // say, an implicit conversion operator is applied to the member.
    // If so, we use the full range of the expr as our range.
    auto range = expr->getMemberNameInfo().getSourceRange();
    if (range.isInvalid()) {
      range = expr->getSourceRange();
    }
    xrefDecl(range, expr->getMemberDecl());
    return true;
  }

  bool VisitCXXDependentScopeMemberExpr(
      const clang::CXXDependentScopeMemberExpr *expr) {
    if (const auto *decl = expr->getFirstQualifierFoundInScope()) {
      xrefTarget(
        expr->getMemberNameInfo().getSourceRange(),
        XRef::unknown(decl));
    }
    return true;
  }

  bool VisitObjCMessageExpr(const clang::ObjCMessageExpr *expr) {
    if (expr->isImplicit()) {
      return true;
    }
    if (const auto *decl = expr->getMethodDecl()) {
      xrefDecl(expr->getSourceRange(), decl);
      auto d = objcMethodDecls(decl);
      for (unsigned int i = 0, n = expr->getNumSelectorLocs(); i < n; ++i) {
        auto target = Cxx::XRefTarget::objcSelectorSlot(
            Cxx::ObjcSelectorSlot{d->decl, i});
        xrefTarget(expr->getSelectorLoc(i), XRef::to(decl, target));
      }
    }
    return true;
  }

  bool VisitObjCPropertyRefExpr(const clang::ObjCPropertyRefExpr *expr) {
    std::vector<const clang::NamedDecl*> decls;
    if (expr->isImplicitProperty()) {
      // Note things like x.foo += 5 generate xrefs to both getter and setter.
      if (expr->isMessagingGetter()) {
        decls.push_back(expr->getImplicitPropertyGetter());
        if (expr->isClassReceiver()) {
          xrefDecl(expr->getReceiverLocation(), expr->getClassReceiver());
        }
      }
      if (expr->isMessagingSetter()) {
        decls.push_back(expr->getImplicitPropertySetter());
      }
    } else {
      decls.push_back(expr->getExplicitProperty());
    }

    const auto loc = expr->getLocation();
    for (const auto* decl : decls) {
      xrefDecl(loc, decl);
    }
    return true;
  }

  bool VisitObjCIvarRefExpr(const clang::ObjCIvarRefExpr *expr) {
    xrefDecl(expr->getLocation(), expr->getDecl());
    return true;
  }

  bool VisitObjCSelectorExpr(const clang::ObjCSelectorExpr* expr) {
    auto [fact, selectors] = objcSelector(expr->getSelector());
    db.xref(
        expr->getSourceRange(),
        Cxx::XRefTarget::objcSelector(fact),
        std::move(selectors),
        true);
    return true;
  }

  bool VisitObjCProtocolExpr(const clang::ObjCProtocolExpr *expr) {
    xrefObjCProtocolDecl(expr->getProtocolIdLoc(), expr->getProtocol());
    return true;
  }

  bool shouldVisitTemplateInstantiations() const {
    return false;
  }

  bool shouldVisitImplicitCode() const {
    return shouldVisitImplicitCode_;
  }

  void finish() {
    db.finish();
  }

  ASTVisitor(ClangDB* d, clang::ASTContext& ctx)
    : db(*d)
    , astContext(ctx)
    , usingTracker(ctx.getTranslationUnitDecl(), *d)
    , declTargets(*d)
    , scopes(*this)
    , namespaces("namespaces", *this)
    , classDecls("classDecls", *this)
    , enumDecls("enumDecls", *this)
    , enumeratorDecls("enumeratorDecls", *this)
    , typeAliasDecls("typeAliasDecls", *this)
    , namespaceAliasDecls("namespaceAliasDecls", *this)
    , funDecls("funDecls", *this)
    , varDecls("varDecls", *this)
    , objcContainerDecls("objcContainerDecls", *this)
    , objcMethodDecls("objcMethodDecls", *this)
    , objcPropertyDecls("objcPropertyDecls", *this)
    {}

  ClangDB& db;
  clang::ASTContext &astContext;

  UsingTracker usingTracker;
  DeclarationTargets declTargets;
  bool shouldVisitImplicitCode_ = false;

  Memo<const clang::DeclContext *,
       Scope,
       &ASTVisitor::defineScope>
    scopes;

  MemoOptional<
      const clang::NamespaceDecl *,
      NamespaceDecl>
    namespaces;

  MemoOptional<
      const clang::CXXRecordDecl *,
      ClassDecl,
      ClassDecl::GetTemplatableDecl>
    classDecls;

  MemoOptional<
      const clang::EnumDecl *,
      EnumDecl,
      EnumDecl::GetTemplatableDecl>
    enumDecls;

  MemoOptional<
      const clang::EnumConstantDecl *,
      EnumeratorDecl>
    enumeratorDecls;

  MemoOptional<
      const clang::TypedefNameDecl*,
      TypeAliasDecl,
      TypeAliasDecl::GetTemplatableDecl>
    typeAliasDecls;

  MemoOptional<
      const clang::NamespaceAliasDecl*,
      NamespaceAliasDecl>
    namespaceAliasDecls;

  MemoOptional<
      const clang::FunctionDecl *,
      FunDecl,
      FunDecl::GetTemplatableDecl>
    funDecls;

  MemoOptional<
      const clang::DeclaratorDecl *,
      VarDecl,
      VarDecl::GetTemplatableDecl>
    varDecls;

  MemoOptional<
      const clang::ObjCContainerDecl *,
        ObjcContainerDecl>
    objcContainerDecls;

  MemoOptional<
      const clang::ObjCMethodDecl *,
      ObjcMethodDecl>
    objcMethodDecls;

  MemoOptional<
      const clang::ObjCPropertyDecl *,
      ObjcPropertyDecl>
    objcPropertyDecls;
    };

// ASTConsumer uses ASTVisitor to traverse the Clang AST
//
struct ASTConsumer : public clang::ASTConsumer {
  explicit ASTConsumer(ClangDB* d) : db(d) {}

  void HandleTranslationUnit (clang::ASTContext &ctx) override {
    // Clang is sometimes generating a bogus AST when there are
    // compilation errors (even for parts of the AST that should be
    // unrelated to the error) so this is a workaround.
    if (ctx.getDiagnostics().hasUncompilableErrorOccurred() &&
        !FLAGS_index_on_error) {
      db->IndexFailure(ctx);
      return;
    }
    ctx.setPrintingPolicy([&ctx] {
      auto policy = ctx.getPrintingPolicy();
      // This adjusted policy makes anonymous types such as lambdas be printed
      // as "(lambda)" rather than "(lambda at /absolute/path/to/file:8:30)".
      //
      // This avoids indexing runs on different machines producing different
      // results, saves a little bit of space, and the location info is
      // generally available in the corresponding source range anyway.
      policy.AnonymousTagLocations = false;
#if LLVM_VERSION_MAJOR >= 11
      // This adjusted policy makes _injected-class-name_ be printed without
      // the template arguments. For example, for `Optional(const Optional&);`
      // The parameter type will be printed as `Optional`, rather than
      // `Optional<Value>`. It's also to achieve consistency in situations where
      // a class template is forward declared with a different type name.
      // `template <typename V> class Optional;` makes the _injected-class-name_
      policy.PrintInjectedClassNameWithArguments = false;
      // This adjusted policy makes types such as `vector<unique_ptr<int>>` to
      // be printed as `vector<unique_ptr<int> >` for consistency with LLVM 9.
      // We should remove this once platform009 is fully gone.
      policy.SplitTemplateClosers = true;
#endif
#if LLVM_VERSION_MAJOR >= 17
      // Without this, the type printer omits scope specifiers
      policy.SuppressElaboration = true;
#endif
      return policy;
    }());
    ASTVisitor visitor(db, ctx);
    VLOG(1) << "traversing";
    visitor.TraverseDecl(ctx.getTranslationUnitDecl());
    visitor.finish();
  }

  ClangDB* db;
};

}

namespace facebook::glean::clangx {

std::unique_ptr<clang::ASTConsumer> newASTConsumer(ClangDB* db) {
  return std::make_unique<ASTConsumer>(db);
}

}
