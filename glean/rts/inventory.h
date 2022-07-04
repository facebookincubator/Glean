/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/fact.h"
#include "glean/rts/id.h"
#include "glean/rts/lookup.h"
#include "glean/rts/substitution.h"
#include "glean/rts/bytecode/subroutine.h"

#include <vector>

namespace facebook {
namespace glean {
namespace rts {

struct Renamer {
  template<typename F>
  explicit Renamer(F f)
    : rename([f = std::move(f)](uint64_t id, uint64_t pid, uint64_t *res) {
        *res = f(Id::fromWord(id), Pid::fromWord(pid)).toWord();
      })
    {
#if __cplusplus >= 201703L
      static_assert(
        std::is_same_v<std::invoke_result_t<F, Id, Pid>, Id>);
#endif
    }

  Renamer(const Renamer&) = delete;

  const std::function<void(uint64_t, uint64_t, uint64_t *)> rename;
};

struct Substituter {
  explicit Substituter(const Substitution* subst)
    : renamer([subst](Id id, Pid) { return subst->subst(id); })
    {}
  explicit Substituter(const Substitution *subst, size_t offset)
    : renamer(
        [subst,offset](Id id, Pid)
          { return id < subst->finish() ? subst->subst(id) : id + offset; })
    {}

  Renamer renamer;
};

struct Traverser {
  template<typename F>
  explicit Traverser(F f)
    : traverse([f = std::move(f)](uint64_t id, uint64_t pid) {
        f(Id::fromWord(id), Pid::fromWord(pid));
      })
    {}

  Traverser(const Traverser&) = delete;

  const std::function<void(uint64_t, uint64_t)> traverse;
};

// NOTE: Any changes here should also be propagated to the internal.thrift
// types and serialize and deserialize should be updated accordingly.
//
// We aren't using the Thrift types here because it isn't clear if those will
// be needed in the long term and it's nice to be able to have methods.

/// Information about a predicate in an open DB.
struct Predicate {
  Pid id;
  std::string name;
  int32_t version;

  /// Typechecker for clauses. It should take the following arguments:
  ///
  /// std::function<Id(Id id, Id type)> - fact substitution
  /// const void * - begin of clause/key
  /// const void * - end of key/begin of value
  /// const void * - end of clause/value
  /// binary::Output * - substituted clause
  /// uint64_t * - size of substituted key
  std::shared_ptr<Subroutine> typechecker;

  /// Generic fact traversal. Takes these arguments:
  ///
  /// std::function<void(Id id, Pid type)> - called for each fact ID
  /// const void * - begin of clause/key
  /// const void * - end of key/begin of value
  /// const void * - end of clause/value
  std::shared_ptr<Subroutine> traverser;

  bool operator==(const Predicate& other) const;
  bool operator!=(const Predicate& other) const {
    return !(*this == other);
  }

  void typecheck(
      const Renamer& renamer,
      Fact::Clause clause,
      binary::Output& output,
      uint64_t& key_size) const {
    runTypecheck(*typechecker, renamer, clause, output, key_size);
  }

  void substitute(
      const Substituter& substituter,
      Fact::Clause clause,
      binary::Output& output,
      uint64_t& key_size) const {
    // TODO: We implement substitution via the typechecker for now but it we
    // might want to generate a more efficient subroutine just for substitution.
    typecheck(substituter.renamer, clause, output, key_size);
  }

  static void runTypecheck(
      const Subroutine& sub,
      const Renamer& renamer,
      Fact::Clause clause,
      binary::Output& output,
      uint64_t& key_size) {
    const uint64_t args[] = {
      reinterpret_cast<uint64_t>(&renamer.rename),
      reinterpret_cast<uint64_t>(clause.data),
      reinterpret_cast<uint64_t>(clause.data + clause.key_size),
      reinterpret_cast<uint64_t>(clause.data + clause.size()),
      reinterpret_cast<uint64_t>(&output),
      reinterpret_cast<uint64_t>(&key_size)};
    sub.execute(args);
  }

  void traverse(
      const Traverser& handler,
      Fact::Clause clause) const {
    runTraverse(*traverser, handler, clause);
  }

  static void runTraverse(
      const Subroutine& sub,
      const Traverser& handler,
      Fact::Clause clause) {
    const uint64_t args[] = {
      reinterpret_cast<uint64_t>(&handler.traverse),
      reinterpret_cast<uint64_t>(clause.data),
      reinterpret_cast<uint64_t>(clause.data + clause.key_size),
      reinterpret_cast<uint64_t>(clause.data + clause.size()),
    };
    sub.execute(args);
  }
};

/// Information about predicates in an open DB.
class Inventory {
public:
  Inventory();

  // The ids in 'predicates' are expected to be mostly dense (gaps are ok for
  // now but the Inventory will use O(max_id - min_id) space.
  explicit Inventory(std::vector<Predicate> predicates);

  const Predicate * FOLLY_NULLABLE lookupPredicate(Pid id) const &;

  Pid firstId() const {
    return first_id;
  }

  Pid firstFreeId() const {
    return firstId() + preds.size();
  }

  // TEMPORARY
  std::vector<const Predicate *> predicates() const;

  std::string serialize() const;
  static Inventory deserialize(folly::ByteRange);

  bool operator==(const Inventory& other) const {
    return first_id == other.first_id
      && preds == other.preds;
  }
  bool operator!=(const Inventory& other) const {
    return !(*this == other);
  }

private:
  Pid first_id;
  std::vector<Predicate> preds;
    // an INVALID Predicate::id means there is no predicate with that id
};

}
}
}
