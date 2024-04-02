/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/ownership/setu32.h"

#include <folly/lang/Assume.h>

#include <xxhash.h>

using namespace folly::compression;

namespace facebook {
namespace glean {
namespace rts {

namespace {

template<typename T>
bool vec_eq(const std::vector<T>& x, const std::vector<T>& y) {
  const auto n = x.size();
  return n == y.size()
    && (n == 0 || std::memcmp(x.data(), y.data(), n * sizeof(x[0])) == 0);
}


template<typename T>
static uint64_t vec_hash(const std::vector<T>& x, uint64_t seed = 0) {
  return XXH64(x.data(), x.size() * sizeof(x[0]), seed);
}


template<typename T>
static size_t vec_bytes(const std::vector<T>& x) {
  return x.size() * sizeof(x[0]);
}

}

bool SetU32::Block::operator==(const SetU32::Block& other) const {
  if (hdr != other.hdr) {
    return false;
  }
  switch (hdr.type()) {
    case SetU32::Hdr::Sparse:
      return std::memcmp(&*sparse, &*other.sparse, hdr.sparseLen()) == 0;
    case SetU32::Hdr::Dense:
      return *dense == *other.dense;
    case SetU32::Hdr::Full:
      return true;
  }
  folly::assume_unreachable();
}

bool SetU32::Block::includes(const SetU32::Block& other) const {
  if (hdr.id() != other.hdr.id()) {
    return false;
  }

  switch (hdr.type()) {
    case SetU32::Hdr::Sparse:
      return other.hdr.type() == SetU32::Hdr::Sparse
        && std::includes(
            sparse,
            sparse + hdr.sparseLen(),
            other.sparse,
            other.sparse + other.hdr.sparseLen());

    case SetU32::Hdr::Dense:
      switch (other.hdr.type()) {
        case SetU32::Hdr::Sparse: {
          const auto v = *dense;
          for (auto i = 0; i < other.hdr.sparseLen(); ++i) {
            if (!v.contains(other.sparse[i])) {
              return false;
            }
          }
          return true;
        }

        case SetU32::Hdr::Dense: {
          return dense->includes(*other.dense);
        }

        case SetU32::Hdr::Full:
          return false;
      }
      break;

    case SetU32::Hdr::Full:
      return true;
  }

  folly::assume_unreachable();
}

SetU32::SetU32(const SetU32& other, SetU32::copy_capacity_tag) {
  reserve(other.capacities());
  hdrs.insert(hdrs.begin(), other.hdrs.begin(), other.hdrs.end());
  dense.insert(dense.begin(), other.dense.begin(), other.dense.end());
  sparse.insert(sparse.begin(), other.sparse.begin(), other.sparse.end());
}

bool SetU32::operator==(const SetU32& other) const {
  return vec_eq(hdrs, other.hdrs)
    && vec_eq(dense, other.dense)
    && vec_eq(sparse, other.sparse);
}


uint64_t SetU32::hash(uint64_t seed) const {
  return vec_hash(sparse, vec_hash(dense, vec_hash(hdrs, seed)));
}

SetU32::Sizes SetU32::sizes() const {
  return {hdrs.size(), dense.size(), sparse.size()};
}

SetU32::Sizes SetU32::capacities() const {
  return {hdrs.capacity(), dense.capacity(), sparse.capacity()};
}

size_t SetU32::bytes() const {
  return vec_bytes(hdrs) + vec_bytes(dense) + vec_bytes(sparse);
}

SetU32::const_iterator SetU32::lower_bound(
    SetU32::const_iterator start,
    SetU32::const_iterator finish,
    uint32_t id) {
  struct Local {
    static std::pair<uint32_t, uint32_t> lengths(
        std::vector<Hdr>::const_iterator first,
        std::vector<Hdr>::const_iterator last) {
      uint32_t dense = 0;
      uint32_t sparse = 0;
      while (first != last) {
          dense += first->denseLen();
          sparse += first->sparseLen();
          ++first;
      }
      return {dense, sparse};
    }
  };

  auto pos = std::lower_bound(
    start.hdrs,
    finish.hdrs,
    id,
    [](auto x, auto id) { return x.before(id); });
  const auto l = pos - start.hdrs;
  const auto r = finish.hdrs - pos;
  if (l <= r) {
    const auto [d,s] = Local::lengths(start.hdrs, pos);
    return {pos, start.block.dense + d, start.block.sparse + s};
  } else {
    const auto [d,s] = Local::lengths(pos, finish.hdrs);
    return {pos, finish.block.dense - d, finish.block.sparse - s};
  }
}

void SetU32::reserve(Sizes sizes) {
  hdrs.reserve(sizes.hdrs);
  dense.reserve(sizes.dense);
  sparse.reserve(sizes.sparse);
}

void SetU32::shrink_to_fit() {
  hdrs.shrink_to_fit();
  dense.shrink_to_fit();
  sparse.shrink_to_fit();
}

size_t SetU32::size() const {
  size_t s = 0;
  for (auto &block : *this) {
    switch (block.hdr.type()) {
      case Hdr::Sparse: {
        s += block.hdr.sparseLen();
        break;
      }
      case Hdr::Dense: {
        s += block.dense->count();
        break;
      }
      case Hdr::Full: {
        s += 256;
        break;
      }
    }
  }
  return s;
}

uint32_t SetU32::upper() const {
  auto &hdr = this->hdrs.back();
  auto id = hdr.id() << 8;
  switch (hdr.type()) {
    case Hdr::Sparse: {
      return id | this->sparse.back();
    }
    case Hdr::Dense: {
      return id | this->dense.back().upper();
    }
    case Hdr::Full: {
      return id | 255;
    }
  }
  folly::assume_unreachable();
}

void SetU32::append(uint32_t value) {
  const auto block = value / 256;
  const auto bit = value % 256;
  if (!hdrs.empty() && hdrs.back().id() == block) {
    auto& hdr = hdrs.back();
    switch (hdr.type()) {
      case Hdr::Sparse:
        assert(bit >= sparse.back());
        if (bit != sparse.back()) {
          const auto len = hdr.sparseLen();
          if (fitsSparse(len, 1)) {
            hdr.addSparseLen(1);
            sparse.push_back(bit);
          } else {
            dense.push_back(Bits256(&*(sparse.end() - len), len) | Bits256::single(bit));
            sparse.resize(sparse.size() - len);
            hdr = Hdr::dense(block);
          }
        }
        break;
      case Hdr::Dense:
        dense.back() |= Bits256::single(bit);
        if (dense.back() == Bits256::all()) {
          dense.pop_back();
          hdr = Hdr::full(block);
        }
        break;

      case Hdr::Full:
        assert(bit == 255);
    }
  } else {
    hdrs.push_back(Hdr::sparse(block, 1));
    sparse.push_back(bit);
  }
}

void SetU32::append(const_iterator start, const_iterator finish) {
  hdrs.insert(hdrs.end(), start.hdrs, finish.hdrs);
  dense.insert(dense.end(), start.block.dense, finish.block.dense);
  sparse.insert(sparse.end(), start.block.sparse, finish.block.sparse);
}

namespace {

enum Which { Left, Right };

std::tuple<const SetU32*, const SetU32*, SetU32::const_iterator, SetU32::const_iterator>
    skipSubset(const SetU32* l, const SetU32* r) {
  auto left = l->begin();
  auto left_end = l->end();
  auto right = r->begin();
  auto right_end = r->end();

  while (left != left_end && right != right_end && *left == *right) {
    ++left;
    ++right;
  }

  bool swapit = false;
  if (right != right_end) {
    if (left == left_end || right.hdrs->id() < left.hdrs->id()) {
      swapit = true;
    } else if(right->includes(*left)) {
      ++left;
      ++right;
      swapit = true;
    }
  }

  if (swapit) {
    std::swap(l,r);
    std::swap(left,right);
    std::swap(left_end,right_end);
  }

  while (left != left_end && right != right_end) {
    if (left.hdrs->id() < right.hdrs->id()) {
      left = SetU32::lower_bound(left, left_end, right.hdrs->id());
    } else if (left->includes(*right)) {
      ++left;
      ++right;
    } else {
      break;
    }
  }
  return {l, r, left, right};
}

}

void SetU32::append(uint32_t id, Bits256 w) {
  if (w == Bits256::all()) {
    hdrs.push_back(Hdr::full(id));
  } else {
    hdrs.push_back(Hdr::dense(id));
    dense.push_back(w);
  }
}

void SetU32::appendMerge(SetU32::Block left, SetU32::Block right) {
  struct Local {
    static void mergeSparse(
        SetU32& set,
        uint32_t id,
        std::vector<uint8_t>::const_iterator ls,
        uint8_t ln,
        std::vector<uint8_t>::const_iterator rs,
        uint8_t rn) {
      uint8_t n = 0;
      while (ln != 0 && rn != 0 && fitsSparse(n,1)) {
        const auto l = *ls;
        const auto r = *rs;
        set.sparse.push_back(l <= r ? l : r);
        ++n;
        if (l <= r) {
          ++ls;
          --ln;
        }
        if (r <= l) {
          ++rs;
          --rn;
        }
      }
      if (fitsSparse(n, ln+rn)) {
        set.hdrs.push_back(Hdr::sparse(id, n+ln+rn));
        set.sparse.insert(set.sparse.end(), ls, ls+ln);
        set.sparse.insert(set.sparse.end(), rs, rs+rn);
      } else {
        const auto dense = Bits256(&*(set.sparse.end()-n),n).with(&*ls,ln).with(&*rs,rn);
        set.hdrs.push_back(Hdr::dense(id));
        set.dense.push_back(dense);
        set.sparse.resize(set.sparse.size()-n);
      }
    }
  };

  switch(left.hdr.type()) {
    case SetU32::Hdr::Sparse:
      switch(right.hdr.type()) {
        case SetU32::Hdr::Sparse:
          Local::mergeSparse(*this, left.hdr.id(), left.sparse, left.hdr.sparseLen(), right.sparse, right.hdr.sparseLen());
          break;

        case SetU32::Hdr::Dense:
          append(left.hdr.id(), right.dense->with(&*left.sparse, left.hdr.sparseLen()));
          break;

        case SetU32::Hdr::Full:
          hdrs.push_back(right.hdr);
          break;
      }
      break;

    case SetU32::Hdr::Dense:
      switch (right.hdr.type()) {
        case SetU32::Hdr::Sparse:
          append(left.hdr.id(), left.dense->with(&*right.sparse, right.hdr.sparseLen()));
          break;

        case SetU32::Hdr::Dense:
          append(left.hdr.id(), *left.dense | *right.dense);
          break;

        case SetU32::Hdr::Full:
          hdrs.push_back(right.hdr);
          break;
      }
      break;

    case SetU32::Hdr::Full:
      hdrs.push_back(left.hdr);
      break;
  }
}

void SetU32::appendMerge(
    SetU32::const_iterator left,
    SetU32::const_iterator left_end,
    SetU32::const_iterator right,
    SetU32::const_iterator right_end) {
  while (left != left_end && right != right_end) {
    if (left.hdrs->id() < right.hdrs->id()) {
      const auto prev = left;
      left = SetU32::lower_bound(left, left_end, right.hdrs->id());
      append(prev,left);
    } else if (right.hdrs->id() < left.hdrs->id()) {
      const auto prev = right;
      right = SetU32::lower_bound(right, right_end, left.hdrs->id());
      append(prev,right);
    } else {
      appendMerge(*left, *right);
      ++left;
      ++right;
    }
  }
  append(left, left_end);
  append(right, right_end);
}

const SetU32 *SetU32::merge(SetU32& result, const SetU32& left, const SetU32& right) {
  if (&left == &right) {
    return &left;
  } else {
    auto [super, sub, super_s, sub_s] = skipSubset(&left, &right);
    if (sub_s == sub->end()) {
      return super;
    } else {
      result.reserve(left.sizes() + right.sizes());
      result.append(super->begin(), super_s);
      result.appendMerge(super_s, super->end(), sub_s, sub->end());
      return &result;
    }
  }
}

SetU32::MutableEliasFanoList SetU32::toEliasFano() {
  auto upperBound = this->upper();
  size_t size = this->size();
  folly::compression::EliasFanoEncoder<uint32_t, uint32_t> encoder(
      size, upperBound);

  VLOG(5) << "upper=" << upperBound << ", size=" << size;
  foreach([&](uint32_t elt) {
    encoder.add(elt);
  });
  return encoder.finish();
}

SetU32 SetU32::fromEliasFano(const EliasFanoList& list) {
  SetU32 set;
  auto reader =
      EliasFanoReader<folly::compression::EliasFanoEncoder<uint32_t, uint32_t>>(
          list);
  while (reader.next()) {
    set.append(reader.value());
  }
  return set;
}

void SetU32::dump(SetU32 &set) {
  for (auto &block : set) {
    auto id = block.hdr.id() << 8;
    switch (block.hdr.type()) {
      case SetU32::Hdr::Sparse: {
        for (uint32_t i = 0; i < block.hdr.sparseLen(); i++) {
          LOG(INFO) << "sparse: " << (id | block.sparse[i]);
        }
        break;
      }
      case SetU32::Hdr::Dense: {
        for (uint32_t i = 0; i < 256; i++) {
          if (block.dense->contains(i)) {
            LOG(INFO) << "dense: " << (id | i);
          }
        }
        break;
      }
      case SetU32::Hdr::Full: {
        for (uint32_t i = 0; i < 256; i++) {
          LOG(INFO) << "full: " << (id | i);
        }
        break;
      }
    }
  }
}

}
}
}
