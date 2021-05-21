#pragma once

#include <immintrin.h>
#include <stdint.h>
#include <algorithm>
#include <cassert>
#include <cstring>
#include <tuple>
#include <vector>
#include <folly/Optional.h>

namespace facebook {
namespace glean {
namespace rts {

/**
 * An implementation of 256-bit bitsets. This uses AVX2, most probably
 * unnecessarily.
 */
struct Bits256 {
  __m256i value;

  Bits256() = default;
  explicit Bits256(__m256i v) : value(v) {}

  Bits256(const uint8_t* vals, uint8_t len) {
    *this = none().with(vals, len);
  }

  /// Check if this set is a superset of the other set
  bool includes(Bits256 other) const {
    return _mm256_testc_si256(value, other.value);
  }

  bool contains(uint8_t n) const {
    return !(*this & single(n)).empty();
  }

  bool empty() const {
    return _mm256_testz_si256(value, value);
  }

  bool operator==(Bits256 other) const {
    return (*this ^ other).empty();
  }

  bool operator!=(Bits256 other) const {
    return !(*this == other);
  }

  static Bits256 none() {
    return Bits256(_mm256_setzero_si256());
  }

  static Bits256 all() {
    return Bits256(_mm256_set1_epi32(-1));
  }

  static Bits256 single(uint8_t n) {
    return Bits256(
      _mm256_sllv_epi32(
        _mm256_set1_epi32(1),
        _mm256_sub_epi32(
          _mm256_set1_epi32(n),
          _mm256_set_epi32(224,192,160,128,96,64,32,0))));
  }

  Bits256 with(const uint8_t *vals, uint8_t len) const {
    // TODO: try to vectorise and/or use lookup table
    auto x = *this;

    for (uint8_t i = 0; i < len; ++i) {
      x |= single(vals[i]);
    }

    return x;
  }

  Bits256 operator|(Bits256 other) const {
    return Bits256(_mm256_or_si256(value, other.value));
  }

  Bits256& operator|=(Bits256 other) {
    *this = *this | other;
    return *this;
  }

  Bits256 operator&(Bits256 other) const {
    return Bits256(_mm256_and_si256(value, other.value));
  }

  Bits256& operator&=(Bits256 other) {
    *this = *this & other;
    return *this;
  }

  Bits256 operator^(Bits256 other) const {
    return Bits256(_mm256_xor_si256(value, other.value));
  }

  Bits256& operator^=(Bits256 other) {
    *this = *this ^ other;
    return *this;
  }
};

/**
 * A memory-efficient representation of sets of uint32_t. This is quite similar
 * to Roaring Bitmaps but with a different branching factor. It's not clear what
 * effect this has - I came up with this independently and didn't have time to
 * measure.
 *
 * We split the uint32_t key space into blocks of 256 values - the upper 24
 * bits determine the block number and the lower 8 bits the index within the
 * block. Each block can be empty, sparse, dense or full. For each non-empty
 * block, we store a 32-bit block header in the the header array. The header
 * contain the 24-bit block id and an 8-bit control byte (`Hdr`) which says what
 * kind of block it is. The header array is sorted by block ids.
 *
 * Empty blocks are not stored in the set. Blocks with less than 32 elements are
 * sparse and are stored as sorted arrays of 8-bit indices (in the `sparse`
 * vector). Blocks with more than 31 but less than 256 elements are dense and
 * are stored as 256-bit bitsets in the `dense` vector. Blocks with 256 elements
 * are full - these blocks are marked separately in the control byte and no
 * additional data is stored.
 *
 * The only two operations on sets that we need are appending a value (which is
 * guaranteed to be >= the largest value in the set) and set union. In
 * particular, we don't need random access which allow us to keep the
 * representation simple.
 */
class SetU32 {
public:
  /**
   * Header control words. This is morally equivalent to
   *
   *   struct Hdr {
   *     unsigned int id: 24;    // block id
   *     unsigned int len: 6;    // number of elements in a sparse block or 0 if
   *                             // this block isn't sparse
   *     unsigned int type: 2;   // type = sparse, dense or full
   *   };
   *
   * However, we do our own bit fiddling since we can do certain things faster.
   * In particular, we try to avoid conditional branches as much as possible.
   */
  struct Hdr {
    enum Type { Sparse = 0, Dense = 1, Full = 2 };

    Hdr() = default;

    static Hdr null() {
      return {0, 0, Sparse};
    }

    static Hdr sparse(uint32_t id, uint8_t len) {
      return {id, len, Sparse};
    }

    static Hdr dense(uint32_t id) {
      return {id, 0, Dense};
    }

    static Hdr full(uint32_t id) {
      return {id, 0, Full};
    }

    uint32_t id() const {
      return value >> 8;
    }

    Type type() const {
      return static_cast<Type>(value & 3);
    }

    /// sparseLen can be called on non-sparse blocks and will be 0 for them
    uint32_t sparseLen() const {
        return (value >> 2) & 63;
    }

    void addSparseLen(uint8_t n) {
      assert(sparseLen() + n < 64);
      value += n << 2;
    }

    /// Number of Bits256 blocks - 1 for dense blocks, 0 otherwise.
    size_t denseLen() const {
        return value & 1;
    }

    bool operator==(Hdr other) const {
      return value == other.value;
    }

    bool operator!=(Hdr other) const {
      return value != other.value;
    }

    bool before(uint32_t id) const {
      return value < (id << 8);
    }

  private:
    Hdr(uint32_t id, uint8_t len, Type type) {
      value = (id << 8) | (uint32_t(len) << 2) | static_cast<uint32_t>(type);
    }

    uint32_t value;
  };

  struct Sizes {
    size_t hdrs = 0;
    size_t dense = 0;
    size_t sparse = 0;

    static Sizes max(const Sizes& x, const Sizes& y) {
      return {std::max(x.hdrs, y.hdrs), std::max(x.dense, y.dense), std::max(x.sparse, y.sparse)};
    }

    Sizes operator+(const Sizes& other) const {
      return {hdrs + other.hdrs, dense + other.dense, sparse + other.sparse};
    }
  };

  SetU32() = default;

  struct copy_capacity_tag {};
  static constexpr copy_capacity_tag copy_capacity;

  SetU32(const SetU32& other, copy_capacity_tag);

  bool operator==(const SetU32& other) const;
  bool operator!=(const SetU32& other) const {
    return !(*this == other);
  }

  uint64_t hash() const;

  Sizes sizes() const;
  Sizes capacities() const;
  size_t bytes() const;

  struct Block {
    Hdr hdr;
    std::vector<Bits256>::const_iterator dense;
    std::vector<uint8_t>::const_iterator sparse;

    bool operator==(const Block& other) const;
    bool operator!=(const Block& other) const {
      return !(*this == other);
    }

    bool includes(const Block& other) const;
  };

  struct const_iterator {
    std::vector<Hdr>::const_iterator hdrs;
    Block block;

    const_iterator(
        std::vector<Hdr>::const_iterator hdrs,
        std::vector<Bits256>::const_iterator dense,
        std::vector<uint8_t>::const_iterator sparse)
      : hdrs(hdrs), block{Hdr::null(), dense, sparse}
      {}

    const Block& operator*() const {
      const_cast<Hdr&>(block.hdr) = *hdrs;
      return block;
    }

    const Block* operator->() const {
      return &operator*();
    }

    const_iterator& operator++() {
      block.sparse += hdrs->sparseLen();
      block.dense += hdrs->denseLen();
      ++hdrs;
      return *this;
    }

    const_iterator operator++(int) {
      const auto x = *this;
      ++*this;
      return x;
    }

    bool operator==(const const_iterator& other) const {
      return hdrs == other.hdrs;
    }

    bool operator!=(const const_iterator& other) const {
      return hdrs != other.hdrs;
    }
  };

  const_iterator begin() const {
    return {
      hdrs.begin(),
      dense.begin(),
      sparse.begin()
    };
  }

  const_iterator end() const {
    return {
      hdrs.end(),
      dense.end(),
      sparse.end()
    };
  }

  static const_iterator lower_bound(
    const_iterator start,
    const_iterator finish,
    uint32_t id);

  void reserve(Sizes sizes);
  void shrink_to_fit();
  void clear();

  /// Append a new value which must be >= the largest value in the set
  void append(uint32_t value);

  /**
   * Merge two sets. If `right` is a subset of `left` or vice versa, returns a
   * pointer to the superset. Otherwise, stores the result in `result` and
   * returns a pointer to it.
   */
  static const SetU32 *merge(SetU32& result, const SetU32& left, const SetU32& right);

private:
  static bool fitsSparse(uint8_t m, uint8_t n) {
    return int(m) + n < 32;
  }

  void append(const_iterator start, const_iterator finish);
  void append(uint32_t id, Bits256 w);

  void appendMerge(
    const_iterator left,
    const_iterator left_end,
    const_iterator right,
    const_iterator right_end);
  void appendMerge(Block left, Block right);

  std::vector<Hdr> hdrs;
  std::vector<Bits256> dense;
  std::vector<uint8_t> sparse;
};

}
}
}
