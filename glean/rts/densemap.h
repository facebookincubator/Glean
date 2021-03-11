#pragma once

#include <vector>
#include <folly/Optional.h>

namespace facebook {
namespace glean {
namespace rts {

/// A map from number-like keys to values with O(1) access but
/// O(highest key - lowest key) space. This is a very basic implementation, it
/// can be vastly improved.
///
/// The interesting operations are operator[] (which dynamically grows the map
/// to cover a particular key) and merge.
template<typename Key, typename Value>
class DenseMap {
public:
  using key_type = Key;
  using mapped_type = Value;

  DenseMap() {}

  void swap(DenseMap& other) {
    std::swap(start, other.start);
    data.swap(other.data);
  }

  bool empty() const {
    return count == 0;
  }

  size_t size() const {
    return count;
  }

private:
  template<typename Val, typename Base>
  struct Iter {
    using value_type = std::pair<const key_type,Val>;
    using reference = std::pair<const key_type,Val&>;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::forward_iterator_tag;

    using base_iterator_type = Base;

    Iter() {}
    Iter(key_type k, base_iterator_type b, base_iterator_type end)
      : key_(std::move(k)), base_(std::move(b)), end_(std::move(end))
    {
      // There is no need to advance to the first available key since we assume
      // that the first and the last one in data are always available
    }

    Iter& operator++() {
      do {
        ++key_;
        ++base_;
      } while (base_ != end_ && !base_->hasValue());
      return *this;
    }

    Iter operator++(int) {
      auto tmp = *this;
      ++*this;
      return tmp;
    }

    reference operator*() const {
      return {key_, base_->value()};
    }

    struct pointer {
      const reference *operator->() const { return &ref; }
      reference ref;
    };

    pointer operator->() const {
      return pointer{{key_, base_->value()}};
    }

    bool operator==(const Iter& other) const {
      return key_ == other.key_ && base_ == other.base_ && end_ == other.end_;
    }

    bool operator!=(const Iter& other) const {
      return !(*this == other);
    }

    key_type key_;
    base_iterator_type base_;
    base_iterator_type end_;
  };

  using repr_type = std::vector<folly::Optional<mapped_type>>;

public:
  using iterator = Iter<mapped_type, typename repr_type::iterator>;
  using const_iterator =
    Iter<const mapped_type, typename repr_type::const_iterator>;

  const_iterator begin() const {
    return const_iterator(start, data.begin(), data.end());
  }

  const_iterator end() const {
    return const_iterator(start + data.size(), data.end(), data.end());
  }

  iterator begin() {
    return iterator(start, data.begin(), data.end());
  }

  iterator end() {
    return iterator(start + data.size(), data.end(), data.end());
  }

private:
  // Reserve space to cover all keys from low up to but not including high
  void reserve(key_type from, key_type upto) {
    if (data.empty()) {
      start = from;
      data.resize(upto - from);
    } else if (from < start) {
      // Jump through some hoops to avoid requiring a copy constructor
      const auto sz = std::max(upto, start + data.size()) - from;
      repr_type xs;
      xs.reserve(sz);
      xs.resize(start - from);
      xs.insert(
          xs.end(),
          std::make_move_iterator(data.begin()),
          std::make_move_iterator(data.end()));
      xs.resize(sz);
      data = std::move(xs);
      start = from;
    } else if (upto - start > data.size()) {
      data.resize(upto - start);
    }
  }

public:
  mapped_type& operator[](key_type key) {
    reserve(key, key+1);
    const auto i = key - start;
    if (!data[i].hasValue()) {
      data[i] = mapped_type();
      ++count;
    }
    return data[i].value();
  }

  folly::Optional<mapped_type> get(key_type key) const {
    if (key >= start) {
      const auto i = key - start;
      if (i < data.size()) {
        return data[i];
      }
    }
    return folly::none;
  }

  mapped_type * FOLLY_NULLABLE lookup(key_type key) {
    if (key >= start) {
      const auto i = key - start;
      if (i < data.size()) {
        return data[i].get_pointer();
      }
    }
    return nullptr;
  }

  const mapped_type * FOLLY_NULLABLE lookup(key_type key) const {
    if (key >= start) {
      const auto i = key - start;
      if (i < data.size()) {
        return data[i].get_pointer();
      }
    }
    return nullptr;
  }

  /// Merge two maps using a combining function of type void(Value&, Value)
  template<typename F>
  void merge(DenseMap other, F&& f) {
    if (!other.data.empty()) {
      if (data.empty()) {
        *this = std::move(other);
      } else {
        reserve(
          std::min(start, other.start),
          std::max(start + data.size(), other.start + other.data.size()));
        auto i = data.begin() + (other.start - start);
        for (auto&& x : other.data) {
          if (x.hasValue()) {
            if (i->hasValue()) {
              f(i->value(), std::move(x.value()));
            } else {
              *i = std::move(x);
              ++count;
            }
          }
          ++i;
        }
      }
    }
  }

private:
  // invariants:
  //
  // !empty() => data.front.hasValue()
  // !empty() => data.back.hasValue()
  key_type start;
  repr_type data;
  size_t count = 0;
};

}
}
}
