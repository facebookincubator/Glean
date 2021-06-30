// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/rts/fact.h"
#include "glean/rts/id.h"
#include "glean/rts/stats.h"

#include <vector>

namespace facebook {
namespace glean {
namespace rts {

/**
 * An iterator for facts.
 *
 */
struct FactIterator {
  enum Demand { KeyOnly, KeyValue };

  // Advance the iterator to the next fact. Calling this after get() returned
  // Fact::Ref::invalid() is not allowed.
  virtual void next() = 0;

  // Get the current fact. Demand says whether to include its value which might
  // be more expensive (rocksdb will do an additional lookup).
  virtual Fact::Ref get(Demand demand = KeyValue) = 0;

  virtual ~FactIterator() {}

  static std::unique_ptr<FactIterator> merge(
    std::unique_ptr<FactIterator> left,
    std::unique_ptr<FactIterator> right,
    size_t prefix_size
  );

  static std::unique_ptr<FactIterator> append(
    std::unique_ptr<FactIterator> left,
    std::unique_ptr<FactIterator> right
  );

  // Filter the facts of the underlying DB according to the provided
  // visibility function. It is the responsibility of the caller to
  // ensure that the resulting set of facts is valid (has no dangling
  // fact IDs).
  static std::unique_ptr<FactIterator> filter(
    std::unique_ptr<FactIterator> base,
    std::function<bool(Id id)> visible
  );
};

/**
 * An iterator which never returns any facts.
 *
 */
struct EmptyIterator final : FactIterator {
  void next() override {}
  Fact::Ref get(Demand) override { return Fact::Ref::invalid(); }
};

/**
 * Interface for looking up fact definitions.
 *
 */
struct Lookup {
  virtual ~Lookup() {}

  // Lookup the Id of a fact by its type and key. Returns Id::INVALID if not
  // found.
  virtual Id idByKey(Pid type, folly::ByteRange key) = 0;

  // Lookup the type of a fact by its id. Returns Id::INVALID if not found.
  virtual Pid typeById(Id id) = 0;

  // Apply the function to the type, key and value of the fact with the given id
  // if it exists. Return value indicates whether the fact has been found.
  // If a type is supplied only facts with this type will be found.
  virtual bool factById(
    Id id, std::function<void(Pid, Fact::Clause)> f) = 0;

  /// Return a fact id such that no facts in the Enumerate have an id below it.
  /// There is no guarantee that a fact with this particular id exists but fact
  /// ids are supposed to be dense between startingId and firstFreeId.
  virtual Id startingId() const = 0;

  /// Return a fact id such that no facts in the Enumerate have an id equal to
  /// or greater than it. See comments on startingId.
  virtual Id firstFreeId() const = 0;

  /// Return bounds on how many facts for a particular predicate the Lookup has.
  virtual Interval count(Pid pid) const = 0;

  /// Return a FactIterator that enumerates all facts in increasing order of
  /// their ids, starting with 'from' and up to, but not including, 'upto'.
  /// Passing `Id::invalid` for either means starting with the first/finishing
  /// with the last fact.
  virtual std::unique_ptr<FactIterator> enumerate(
    Id from = Id::invalid(),
    Id upto = Id::invalid()) = 0;

  /// Return a FactIterator that enumerates all facts in descreasing order of
  /// their ids, starting from the first fact before 'from' and down to and
  /// including 'downto'. Passing `Id::invalid` for either means starting with
  /// the last/finishing with the first fact.
  ///
  /// Note that `from` is typically the fact id *one past* the first fact
  /// returned by the iterator and `downto` the fact id of the last fact
  /// returned by the iterator. This means that `enumerateBack(b,a)` produces
  /// exactly the same facts as `enumerate(a,b)`, just in reverse order.
  virtual std::unique_ptr<FactIterator> enumerateBack(
    Id from = Id::invalid(),
    Id downto = Id::invalid()) = 0;

  // Obtain a  prefix iterator for the given predicate. The iterator covers keys
  // which begin with the first 'prefix_size' bytes of 'start', starting with
  // the first key not lexicographically less than 'start', and produces facts
  // in lexicographic order. In particular, setting 'prefix_size' to
  // 'start.size()' iterates over all keys with the prefix 'start'.
  virtual std::unique_ptr<FactIterator> seek(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size
  ) = 0;
};

/**
 * An implementation of Lookup which doesn't find any facts.
 *
 */
struct EmptyLookup final : Lookup {
  Id idByKey(Pid, folly::ByteRange) override {
    return Id::invalid();
  }

  Pid typeById(Id) override {
    return Pid::invalid();
  }

  bool factById(Id, std::function<void(Pid, Fact::Clause)>) override {
    return false;
  }

  Id startingId() const override { return Id::lowest(); }
  Id firstFreeId() const override { return Id::lowest(); }

  Interval count(Pid) const override { return 0; }

  std::unique_ptr<FactIterator> enumerate(Id, Id) override {
    return std::make_unique<EmptyIterator>();
  }
  std::unique_ptr<FactIterator> enumerateBack(Id, Id) override {
    return std::make_unique<EmptyIterator>();
  }

  std::unique_ptr<FactIterator> seek(Pid, folly::ByteRange, size_t) override
  {
    return std::make_unique<EmptyIterator>();
  }

  static EmptyLookup& instance();
};

/**
 * A Lookup which ignores all facts with Ids from the given one up. Since we
 * assign Ids sequentially, this effectively implements a snapshot of the
 * database.
 *
 */

struct Snapshot : Lookup {
  Snapshot(Lookup *b, Id i) : base_(b), boundary_(i) {}

  Id idByKey(Pid type, folly::ByteRange key) override {
    auto id = base()->idByKey(type, key);
    return id < boundary() ? id : Id::invalid();
  }

  Pid typeById(Id id) override {
    return id < boundary() ? base()->typeById(id) : Pid::invalid();
  }

  bool factById(Id id, std::function<void(Pid, Fact::Clause)> f) override {
    return id < boundary() && base()->factById(id, std::move(f));
  }

  Id startingId() const override {
    return std::min(base()->startingId(), boundary());
  }

  Id firstFreeId() const override {
    return std::min(boundary(), base()->firstFreeId());
  }

  Interval count(Pid pid) const override {
    return base_->count(pid).asHigh();
  }

  std::unique_ptr<FactIterator> enumerate(Id from, Id upto) override;
  std::unique_ptr<FactIterator> enumerateBack(Id from, Id downto) override;

  std::unique_ptr<FactIterator> seek(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size) override;

  Lookup *base() const {
    return base_;
  }

  Id boundary() const {
    return boundary_;
  }

private:
  Lookup *base_;
  Id boundary_;
};

}
}
}
