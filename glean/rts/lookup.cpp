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

namespace {

struct MergeIterator final : public FactIterator {
  MergeIterator(
      std::unique_ptr<FactIterator> l,
      std::unique_ptr<FactIterator> r,
      size_t pfxsize)
    : left(std::move(l))
    , right(std::move(r))
    , current(nullptr)
    , prefix_size(pfxsize)
  {}

  void next() override {
    if (!current) {
      get(KeyOnly);
    }
    current->next();
    current = nullptr;
  }

  Fact::Ref get(Demand demand) override {
    if (current) {
      return current->get(demand);
    } else {
      auto l = tryGet(left, demand);
      auto r = tryGet(right, demand);
      if (l) {
        if (r && suffix(r) < suffix(l)) {
          current = right.get();
          return r;
        } else {
          current = left.get();
          return l;
        }
      } else {
        current = right.get();
        return r;
      }
    }
  }

  std::optional<Id> lower_bound() override {
    auto l = left ? left->lower_bound() : std::nullopt;
    auto r = right ? right->lower_bound() : std::nullopt;
    if (!l) {
      return r;
    }
    return r ? std::min(*l, *r) : l;

  }

  std::optional<Id> upper_bound() override {
    auto l = left ? left->upper_bound() : std::nullopt;
    auto r = right ? right->upper_bound() : std::nullopt;
    if (!l) {
      return r;
    }
    return r ? std::max(*l, *r) : l;
  }

  static Fact::Ref tryGet(std::unique_ptr<FactIterator>& iter, Demand demand) {
    Fact::Ref ref;
    if (iter) {
      ref = iter->get(demand);
      if (!ref) {
        iter.reset();
      }
    }
    return ref;
  }

  folly::ByteRange suffix(Fact::Ref ref) {
    assert(prefix_size <= ref.key().size());
    return {ref.key().begin() + prefix_size, ref.key().end()};
  }

  std::unique_ptr<FactIterator> left;
  std::unique_ptr<FactIterator> right;
  FactIterator * FOLLY_NULLABLE current;
  const size_t prefix_size;
};

}

std::unique_ptr<FactIterator> FactIterator::merge(
    std::unique_ptr<FactIterator> left,
    std::unique_ptr<FactIterator> right,
    size_t prefix_size) {
  if (left->get(Demand::KeyOnly)) {
    return right->get(Demand::KeyOnly)
      ? std::make_unique<MergeIterator>(
          std::move(left), std::move(right), prefix_size)
      : std::move(left);
  } else {
    return right;
  }
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
    Pid type, folly::ByteRange start, size_t prefix_size) {
  struct Iterator final : FactIterator {
    Iterator(std::unique_ptr<FactIterator> base, Id upto, Id from)
      : base_(std::move(base)), high_boundary_(upto), low_boundary_(from)
      {}

    void next() override { base_->next(); }

    Fact::Ref get(Demand demand) override {
      auto r = base_->get(demand);

      while (r && !isWithinBounds(r.id)) {
        r = base_->get(demand);
      }

      return r;
    }

    std::optional<Id> lower_bound() override { return low_boundary_; }
    std::optional<Id> upper_bound() override { return high_boundary_; }

    bool isWithinBounds(Id id) {
      return low_boundary_ <= id && id < high_boundary_;
    }

    std::unique_ptr<FactIterator> base_;
    Id high_boundary_;
    Id low_boundary_;
  };
  return std::make_unique<Iterator>(
    base()->seek(type, start, prefix_size),
    highBoundary(),
    lowBoundary());
}

std::unique_ptr<FactIterator> Section::seekWithinSection(
    Pid type, folly::ByteRange start, size_t prefix_size, Id from, Id upto) {
  if (from <= lowBoundary() && highBoundary() <= upto) {
    return seek(type, start, prefix_size);
  } else {
    return Section(base_, from, upto).seek(type, start, prefix_size);
  }
}

namespace {

struct AppendIterator final : FactIterator {
  AppendIterator(
    std::unique_ptr<FactIterator> l,
    std::unique_ptr<FactIterator> r)
  : current(std::move(l))
  , other(std::move(r))
  , checked(false)
  {}

  void next() override {
    if (!checked) {
      get(KeyOnly);
    }
    current->next();
    checked = false;
  }

  Fact::Ref get(Demand demand) override {
    checked = true;
    auto r = current->get();
    if (!r && other) {
      std::swap(current,other);
      other.reset();
      r = current->get();
    }
    return r;
  }

  std::optional<Id> lower_bound() override { return std::nullopt; }
  std::optional<Id> upper_bound() override { return std::nullopt; }
  std::unique_ptr<FactIterator> current;
  std::unique_ptr<FactIterator> other;
  bool checked;
};

}

std::unique_ptr<FactIterator> FactIterator::append(
    std::unique_ptr<FactIterator> left,
    std::unique_ptr<FactIterator> right) {
  return
    std::make_unique<AppendIterator>(std::move(left), std::move(right));
}

namespace {

struct FilterIterator final : FactIterator {
  FilterIterator(
    std::unique_ptr<FactIterator> base,
    std::function<bool(Id id)> visible)
      : base_(std::move(base)),
        visible_(std::move(visible)) {}

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

  std::optional<Id> lower_bound() override { return base_->lower_bound(); }
  std::optional<Id> upper_bound() override { return base_->upper_bound(); }
  std::unique_ptr<FactIterator> base_;
  std::function<bool(Id id)> visible_;
};

} // namespace

std::unique_ptr<FactIterator> FactIterator::filter(
    std::unique_ptr<FactIterator> base,
    std::function<bool(Id id)> visible) {
  return
    std::make_unique<FilterIterator>(
        std::move(base),
        std::move(visible));
}

std::unique_ptr<Lookup> snapshot(Lookup *b, Id upto) {
  return std::make_unique<Section>(Section(b, Id::invalid(), upto));
}

}
}
}
