/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/lookup.h"

namespace facebook {
namespace glean {
namespace rts {

EmptyLookup& EmptyLookup::instance() {
  static EmptyLookup object;
  return object;
}

std::unique_ptr<FactIterator> Section::enumerate(Id from, Id upto) {
  if (upto <= lowBoundary() || highBoundary() <= from) {
    return std::make_unique<EmptyIterator>();
  } else {
    return base()->enumerate(
        std::max(from, lowBoundary()),
        upto ? std::min(upto, highBoundary()) : highBoundary());
  }
}

std::unique_ptr<FactIterator> Section::enumerateBack(Id from, Id downto) {
  if (from <= lowBoundary() || highBoundary() <= downto) {
    return std::make_unique<EmptyIterator>();
  } else {
    return base()->enumerate(
        from ? std::min(from, highBoundary()) : highBoundary(),
        std::max(downto, lowBoundary()));
  }
}

std::unique_ptr<FactIterator> Section::seek(
    Pid type,
    folly::ByteRange prefix,
    std::optional<Fact::Ref> restart) {
  struct Iterator final : FactIterator {
    Iterator(std::unique_ptr<FactIterator> base, Id upto, Id from)
        : base_(std::move(base)), high_boundary_(upto), low_boundary_(from) {}

    void next() override {
      base_->next();
    }

    Fact::Ref get(Demand demand) override {
      auto r = base_->get(demand);

      while (r && !isWithinBounds(r.id)) {
        r = base_->get(demand);
      }

      return r;
    }

    std::optional<Id> lower_bound() override {
      return low_boundary_;
    }
    std::optional<Id> upper_bound() override {
      return high_boundary_;
    }

    bool isWithinBounds(Id id) {
      return low_boundary_ <= id && id < high_boundary_;
    }

    std::unique_ptr<FactIterator> base_;
    Id high_boundary_;
    Id low_boundary_;
  };
  return std::make_unique<Iterator>(
      base()->seek(type, prefix, restart), highBoundary(), lowBoundary());
}

std::unique_ptr<FactIterator> Section::seekWithinSection(
    Pid type,
    folly::ByteRange prefix,
    Id from,
    Id upto,
    std::optional<Fact::Ref> restart) {
  if (from <= lowBoundary() && highBoundary() <= upto) {
    return seek(type, prefix, restart);
  } else {
    return Section(base_, from, upto).seek(type, prefix, restart);
  }
}

namespace {

struct AppendIterator final : FactIterator {
  AppendIterator(
      std::unique_ptr<FactIterator> l,
      std::unique_ptr<FactIterator> r)
      : current(std::move(l)), other(std::move(r)), checked(false) {}

  void next() override {
    if (!checked) {
      get(KeyOnly);
    }
    current->next();
    checked = false;
  }

  Fact::Ref get(Demand demand) override {
    checked = true;
    auto r = current->get(demand);
    if (!r && other) {
      std::swap(current, other);
      other.reset();
      r = current->get(demand);
    }
    return r;
  }

  std::optional<Id> lower_bound() override {
    return std::nullopt;
  }
  std::optional<Id> upper_bound() override {
    return std::nullopt;
  }
  std::unique_ptr<FactIterator> current;
  std::unique_ptr<FactIterator> other;
  bool checked;
};

} // namespace

std::unique_ptr<FactIterator> FactIterator::append(
    std::unique_ptr<FactIterator> left,
    std::unique_ptr<FactIterator> right) {
  return std::make_unique<AppendIterator>(std::move(left), std::move(right));
}

namespace {

struct FilterIterator final : FactIterator {
  FilterIterator(
      std::unique_ptr<FactIterator> base,
      std::function<bool(Id id)> visible)
      : base_(std::move(base)), visible_(std::move(visible)) {}

  void next() override {
    base_->next();
  }

  Fact::Ref get(Demand demand) override {
    // TODO: If we're doing a prefix seek and demand requires values, this
    // will do 2 DB lookups (to get the value) even for facts that we
    // skip which can be pretty significant. One possibility to avoid
    // this is to do a KeyOnly lookup first, check the fact id and do
    // a KeyValue lookup (if necessary) only if we want the
    // fact. Another is to push the filtering all the way down into
    // the individual iterators (ugly).
    auto r = base_->get(demand);
    while (r.id != Id::invalid() && !visible_(r.id)) {
      base_->next();
      r = base_->get(demand);
    }
    return r;
  }

  std::optional<Id> lower_bound() override {
    return base_->lower_bound();
  }
  std::optional<Id> upper_bound() override {
    return base_->upper_bound();
  }
  std::unique_ptr<FactIterator> base_;
  std::function<bool(Id id)> visible_;
};

} // namespace

std::unique_ptr<FactIterator> FactIterator::filter(
    std::unique_ptr<FactIterator> base,
    std::function<bool(Id id)> visible) {
  return std::make_unique<FilterIterator>(std::move(base), std::move(visible));
}

std::unique_ptr<Lookup> snapshot(Lookup* b, Id upto) {
  return std::make_unique<Section>(Section(b, Id::invalid(), upto));
}

} // namespace rts
} // namespace glean
} // namespace facebook
