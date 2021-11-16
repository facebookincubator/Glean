/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * A very simple types C++ interface to Glean batches.
 *
 */

#pragma once

#include <tuple>
#include <boost/operators.hpp>
#include <boost/variant.hpp>
#include <folly/Format.h>

#include "glean/rts/binary.h"
#include "glean/rts/cache.h"
#include "glean/rts/inventory.h"
#include "glean/rts/id.h"
#include "glean/rts/stacked.h"

namespace facebook {
namespace glean {
namespace cpp {

using rts::Id;
using rts::Pid;

// There are two kinds of types:
//   * Representation types
//   * Value types
//
// Representation Types
// --------------------
//
// These aren't real values, they're just tags which can be filled
// by different types. For instance, a Tuple<Nat,Nat> might be filled by
// std::tuple<uint64_t, size_t> and an Array<Byte> might be filled by
// std::string, fbstring etc.
//
// The following types are supported:
//
// Byte                 bytes
//
// Nat                  packed natural numbers (up to 64 bits)
//
// Array<T>             arrays
//
//  Tuple<...>           tuples
//   Unit = Tuple<>
//
// Sum<...>             sum types
//   Maybe<T> = Sum<Unit,T>
//
// Enum<N>              enums with range N
//                      (map to sums underneath)
//   Bool = Enum<2>
//
// Predicates, e.g.:
//     struct Foo : Predicate<KeyType, ValueType> {
//       static const char *name() { return "Foo"; }
//     };
//     NOTE: KeyType and ValueType are *value* types. This is to
//     increase type safety when constructing facts: in fact<P>(x),
//     x must have the right type, not just the right shape.
//
//
// Value Types
// -----------
//
// Value types are inhabited instances of representation types.
// A value type has a single representation type, however there
// are multiple possible value types for a given representation.
// e.g. for the representation Array<Byte>, value types might be
// std::string or std::vector<uint8_t>.
//
// std::vector<T>
//   value type corresponding to Array<Repr<T>>
//
// std::tuple<Xs...>
//   value type corresponding to Tuple<Repr<Xs>...>
//
// facts
//    Fact<P> is a reference to a fact of predicate P
//
// enumerated types
//    enum class Foo { thing1, thing2 };
//    template<> struct Repr_<Foo> {
//      using Type = Enum<2>;
//    }
//
// user-defined         arbitrary types which map to any representation type
//    struct Foo {
//      uint64_t x,y;
//      void outputRepr(Output<Repr<Foo>> output) const {
//        outputValue(output, std::make_pair(x,y));
//      }
//    };
//    template<> struct Repr_<Foo> {
//      using Type = Tuple<Nat,Nat>;
//    }
//
//
// The Repr<T> template
// --------------------
//
// Repr<V> is the representation type corresponding to the
// value type V.
//
// Repr<V> is defined for:
//   * user-defined structs
//   * enumerated types

template<typename T> struct Repr_;

// Convenience:
template<typename T> using Repr = typename Repr_<T>::Type;

struct Byte;

template<> struct Repr_<unsigned char> { using Type = Byte; };

struct Nat;

template<> struct Repr_<unsigned long> { using Type = Nat; };

struct String;

template<> struct Repr_<std::string> { using Type = String; };

template<typename T> struct Array;

template<typename T>
struct Repr_<std::vector<T>> {
  using Type = Array<Repr<T>>;
};

template<typename... Ts> struct Tuple;
using Unit = Tuple<>;

template<typename... Ts>
struct Repr_<std::tuple<Ts...>> {
  using Type = Tuple<Repr<Ts>...>;
};

template<typename... Ts> struct Sum;
template<typename T>
using Maybe = Sum<Unit,T>;

template<size_t i, typename T>
struct Alt : private boost::operators<Alt<i,T>> {
  T value;
  explicit Alt(const T& x) : value(x) {}
  explicit Alt(T&& x) : value(std::forward<T>(x)) {}

  bool operator==(const Alt& other) const {
    return value == other.value;
  }

  bool operator<(const Alt& other) const {
    return value < other.value;
  }
};

template<size_t... is, typename... Ts>
struct Repr_<boost::variant<Alt<is,Ts>...>> {
  using Type = Sum<Repr<Ts>...>;
};

template<size_t i, typename T>
Alt<i,std::decay_t<T>> alt(T&& x) {
  return Alt<i,std::decay_t<T>>(std::forward<T>(x));
}

inline Alt<0,std::tuple<>> nothing() {
  return alt<0>(std::make_tuple());
}

template<typename T>
Alt<1,std::decay_t<T>> just(T&& x) {
  return alt<1>(std::forward<T>(x));
}

template<typename T> using maybe_type =
  boost::variant<Alt<0, std::tuple<>>, Alt<1,T>>;

template<typename T>
maybe_type<T> maybe(const folly::Optional<T>& x) {
  if (x) {
    return alt<1>(x.value());
  } else {
    return alt<0>(std::make_tuple());
  }
}

// Enums - a special case of sums
//
// T is the actual value type, N is the range of the enum
template<size_t N> struct Enum {};
using Bool = Enum<2>;
template<> struct Repr_<bool> { using Type = Bool; };

// Predicates
//
// Predicates are type tags. Fact<P> (see below) is the corresponding value.
//
// A predicate is reflected into C++ as follows:
//
// struct Foo : Predicate<Tuple<Nat,Nat>> {
//   static const char *name() { return "foo"; }
// };
//
// struct Bar : Predicate<Foo,Array<Byte>> {
//   static const char *name() { return "bar"; }
// };
//
template<typename Key, typename Value = std::tuple<>>
struct Predicate {
  using KeyType = Key;
  using ValueType = Value;
};

// Facts
template<typename P> struct Fact : private boost::operators<Fact<P>> {
  Id getId() const { return id; }

  bool operator==(const Fact& other) const {
    return id == other.id;
  }

  bool operator<(const Fact& other) const {
    return id < other.id;
  }

private:
  template<typename Schema>
  friend class Batch;
  explicit Fact(Id i) : id(i) {}
  Id id;
};

// The representation type of a Fact is its predicate:
template<typename P> struct Repr_<Fact<P>> { using Type = P; };

// Typed value output buffers
template<typename T>
struct Output {
  binary::Output& output;

  explicit Output(binary::Output& o) : output(o) {}
};

template<typename T, typename U>
Output<T> unsafeAs(Output<U> o) {
  return Output<T>(o.output);
}

inline void outputValue(Output<Byte> o, uint8_t x) {
  o.output.fixed(x);
}

inline void outputValue(Output<Nat> o, uint64_t x) {
  o.output.packed(x);
}

inline void outputValue(Output<String> o, const std::string& s) {
  // TODO: Validate UTF-8?
  o.output.mangleString(binary::byteRange(s));
}

template<typename T, typename U>
void outputValue(Output<Array<T>> o, std::initializer_list<U> xs) {
  o.output.packed(xs.size());
  for (const auto& x : xs) {
    outputValue(unsafeAs<T>(o), x);
  }
}

template<typename T, typename U>
void outputValue(Output<Array<T>> o, const std::vector<U>& xs) {
  o.output.packed(xs.size());
  for (const auto& x : xs) {
    outputValue(unsafeAs<T>(o), x);
  }
}

template<typename T, typename Iter>
void outputValue(Output<Array<T>> o, const folly::Range<Iter>& range) {
  o.output.packed(range.size());
  for (const auto& x : range) {
    outputValue(unsafeAs<T>(o), x);
  }
}

inline void outputValue(Output<Array<Byte>> o, folly::ByteRange r) {
  o.output.packed(r.size());
  o.output.put(r);
}

inline void outputValue(Output<Array<Byte>> o, const std::string& s) {
  outputValue(o, binary::byteRange(s));
}

inline void outputValue(Output<Array<Byte>> o, const folly::fbstring& s) {
  outputValue(o, binary::byteRange(s));
}

namespace detail {

template<size_t i, typename U>
void outputValues(binary::Output&, const U&) {}

template<size_t i, typename U, typename T, typename... Ts>
void outputValues(binary::Output& output, const U& u) {
  outputValue(Output<T>(output), std::get<i>(u));
  outputValues<i+1, U, Ts...>(output,u);
}

}

template<
  typename... Ts,
  typename... Us,
  typename = std::enable_if_t<sizeof...(Ts) == sizeof...(Us)>>
void outputValue(Output<Tuple<Ts...>> o, const std::tuple<Us...>& xs) {
  detail::outputValues<0, std::tuple<Us...>, Ts...>(o.output, xs);
}

template<
  size_t i,
  typename U,
  typename... Ts,
  typename = std::enable_if_t<i < sizeof...(Ts)>>
void outputValue(Output<Sum<Ts...>> o, const Alt<i,U>& alt) {
  o.output.packed(i);
  outputValue(
    unsafeAs<typename std::tuple_element<i, std::tuple<Ts...>>::type>(o),
    alt.value);
}

namespace {

template<typename... Ts>
struct OutputAlt : public boost::static_visitor<void> {
  explicit OutputAlt(Output<Sum<Ts...>> o) : out(o) {}

  template<size_t i, typename U>
  void operator()(const Alt<i,U>& alt) const {
    outputValue(out,alt);
  }

  Output<Sum<Ts...>> out;
};

}

template<
  typename... Ts,
  typename... Us,
  typename = std::enable_if_t<sizeof...(Ts) == sizeof...(Us)>>
void outputValue(Output<Sum<Ts...>> o, const boost::variant<Us...>& v) {
  boost::apply_visitor(OutputAlt<Ts...>(o), v);
}

template<typename T, typename U>
void outputValue(Output<Sum<Unit,T>> o, const folly::Optional<U>& x) {
  if (x) {
    o.output.packed(1);
    outputValue(unsafeAs<T>(o), x.value());
  } else {
    o.output.packed(0);
  }
}

template<typename T>
void outputValue(Output<Sum<Unit,T>> o, folly::None) {
  o.output.packed(0);
}

template<typename T, size_t N>
void outputValue(Output<Enum<N>> o, T x) {
  uint64_t n = static_cast<uint64_t>(x);
  assert (n < N);
  outputValue(unsafeAs<Nat>(o), n);
}

template<typename P>
void outputValue(Output<P> o, Fact<P> fact) {
  o.output.packed(fact.getId());
}

template<typename T>
void outputValue(Output<Repr<T>> o, const T& x) {
  x.outputRepr(o);
}

struct FactStats {
  size_t memory;
  size_t count;
};


struct SchemaInventory {
  rts::Inventory inventory;

  /// Mapping from the autogenerated Schema::index<P>::value to the Id of P
  /// in the inventory (if it exists there).
  std::vector<const rts::Predicate * FOLLY_NULLABLE> predicates;
};

class BatchBase {
public:
  explicit BatchBase(
    const SchemaInventory *inventory,
    size_t cache_capacity);
  BatchBase(BatchBase&&) = default;
  BatchBase& operator=(BatchBase&&) = default;

  BatchBase(const BatchBase&) = delete;
  BatchBase& operator=(const BatchBase&) = delete;

  const rts::Predicate * FOLLY_NULLABLE predicate(size_t i) const {
    return inventory->predicates[i];
  }

  Id define(Pid ty, rts::Fact::Clause clause) {
    return facts.define(ty, clause);
  }

  thrift::Batch serialize() const;
  void rebase(const thrift::Subst&);

  FactStats bufferStats() const {
    return FactStats{buffer.factMemory(), buffer.size()};
  }

  // TODO: This is a temporary hack for backwards compatibility, add proper
  // stats reporting
  struct CacheStats {
    FactStats facts;
    size_t hits = 0;
    size_t misses = 0;
  };

  CacheStats cacheStats();

private:
  const SchemaInventory *inventory;
  std::shared_ptr<rts::LookupCache::Stats> stats;
  rts::LookupCache cache;
  rts::LookupCache::Anchor anchor;
  rts::FactSet buffer;
  rts::Stacked<rts::Define> facts;
};

/// A typed instantiation of an Inventory for a particular Schema.
template<typename Schema>
struct DbSchema {
  SchemaInventory inventory;

  explicit DbSchema(rts::Inventory inv)
    : inventory{std::move(inv), {}}
    {
      inventory.predicates = getPredicates(inventory.inventory);
    }

private:
  using ref_t = std::pair<std::string, size_t>;

  template<size_t i> using pred_t =
    typename Schema::template predicate<i>::type;

  template<size_t... I> static std::vector<ref_t> getRefs(
      std::index_sequence<I...>) {
    return std::vector<ref_t>{
      ref_t{pred_t<I>::GLEAN_name(),pred_t<I>::GLEAN_version()}...};
  }

  using pred_map_t =
    std::unordered_map<std::pair<std::string, int32_t>, size_t>;

  static std::vector<const rts::Predicate * FOLLY_NULLABLE>
      getPredicates(const rts::Inventory& inventory) {
    const auto seq = std::make_index_sequence<Schema::count>();
    auto refs = getRefs(seq);

    std::unordered_map<ref_t, size_t> indices;
    for (size_t i = 0; i < refs.size(); ++i) {
      indices.insert({refs[i], i});
    }

    const auto inventory_preds = inventory.predicates();
    std::vector<const rts::Predicate * FOLLY_NULLABLE>
      preds(refs.size(), nullptr);
    for (auto p : inventory_preds) {
      auto i = indices.find({p->name, p->version});
      if (i != indices.end()) {
        preds[i->second] = p;
      }
    }

    // TODO: verify that predicates have the expected types

    return preds;
  }
};

// A Batch encapsulates a set of cached facts which is initially empty and an
// set of facts local to the Batch. Local facts can reference cached ones but
// not vice versa. New local facts can be added to the Batch.
//
// A Batch can be serialized (typically for sending to the server) and rebased
// based on a Substitution (typically received from the server). Local facts
// that are in range of the substitution are moved to the cache; the remaining
// local facts are assigned new Ids which don't clash which cached ones.
//
template<typename Schema>
class Batch : private BatchBase {
public:
  Batch(const DbSchema<Schema>* schema, size_t cache_capacity)
    : BatchBase(&(schema->inventory), cache_capacity)
    {}

  template<typename P>
  const rts::Predicate *predicate() const {
    if (auto p = base().predicate(Schema::template index<P>::value)) {
      return p;
    } else {
      throw std::runtime_error(
        std::string("unknown predicate ") + P::GLEAN_name()
        + "[" + folly::to<std::string>(P::GLEAN_version()) + "]");
    }
  }

  template<
    typename P,
    typename =
      std::enable_if_t<
        std::is_same<typename P::ValueType, std::tuple<>>::value>>
  Fact<P> fact(const typename P::KeyType& x) {
    binary::Output output;
    outputValue(Output<Repr<typename P::KeyType>>(output), x);
    auto fact = base().define(
      predicate<P>()->id, rts::Fact::Clause::fromKey(output.bytes()));
    assert(fact);
    return Fact<P>(fact);
  }

  template<
    typename P,
    typename... Ts,
    typename = std::enable_if_t<1 < sizeof...(Ts)>>
  Fact<P> fact(Ts&&... xs) {
    return fact<P>(std::make_tuple(std::forward<Ts>(xs)...));
  }

  template<typename P>
  Fact<P> factV(const typename P::KeyType& k, const typename P::ValueType& v) {
    binary::Output clause;
    outputValue(Output<Repr<typename P::KeyType>>(clause), k);
    const auto key_size = clause.size();
    outputValue(Output<Repr<typename P::ValueType>>(clause), v);
    auto fact = base().define(
      predicate<P>()->id, rts::Fact::Clause::from(clause.bytes(), key_size));
    assert(fact);
    return Fact<P>(fact);

  }

  BatchBase& base() { return *this; }
  const BatchBase& base() const { return *this; }

  using BatchBase::serialize;
  using BatchBase::rebase;
  using BatchBase::bufferStats;
  using BatchBase::CacheStats;
  using BatchBase::cacheStats;
};

}
}
}
