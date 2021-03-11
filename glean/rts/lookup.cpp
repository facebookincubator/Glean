#include "glean/rts/lookup.h"
#include "glean/rts/error.h"

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


std::unique_ptr<FactIterator> Snapshot::enumerate(Id from, Id upto) {
  if (from >= boundary()) {
    return std::make_unique<EmptyIterator>();
  } else {
    return base()->enumerate(
      from,
      upto ? std::min(upto, boundary()) : boundary());
  }
}

std::unique_ptr<FactIterator> Snapshot::enumerateBack(Id from, Id downto) {
  if (downto >= boundary()) {
    return std::make_unique<EmptyIterator>();
  } else {
    return base()->enumerate(
      from ? std::min(from, boundary()) : boundary(),
      downto);
  }
}

std::unique_ptr<FactIterator> Snapshot::seek(
    Pid type, folly::ByteRange start, size_t prefix_size) {
  struct Iterator final : FactIterator {
    Iterator(std::unique_ptr<FactIterator> base, Id boundary)
      : base_(std::move(base)), boundary_(boundary)
      {}

    void next() override { base_->next(); }

    Fact::Ref get(Demand demand) override {
      auto r = base_->get(demand);
      return r && r.id < boundary_ ? r : Fact::Ref::invalid();
    }

    std::unique_ptr<FactIterator> base_;
    Id boundary_;
  };
  return std::make_unique<Iterator>(
    base()->seek(type, start, prefix_size),
    boundary());
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

}
}
}
