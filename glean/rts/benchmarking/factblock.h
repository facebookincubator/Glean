/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/define.h"

#include <boost/iterator/transform_iterator.hpp>

namespace facebook {
namespace glean {
namespace rts {
namespace benchmarking {

struct FactBlock {
  Id starting_id;

  struct Ref {
    Pid type;
    size_t offset;
    uint32_t key_size;
    uint32_t value_size;
  };

  std::vector<Ref> refs;
  std::vector<unsigned char> data;

  static FactBlock create(FactIterator& iterator);

  // std::unique_ptr<FactIterator> enumerate() const;

  struct deref {
    Id starting_id;
    const Ref *refs;
    const unsigned char *data;
    const size_t count;
    Fact::Ref operator()(const FactBlock::Ref& r) const {
      assert(&r >= refs && &r < refs + count);
      return Fact::Ref{
        starting_id + (&r - refs),
        r.type,
        Fact::Clause{data + r.offset, r.key_size, r.value_size
        }
      };
    }
  };

  using const_iterator =
    boost::transform_iterator<deref, std::vector<Ref>::const_iterator>;

  const_iterator iterator(std::vector<Ref>::const_iterator base) const {
    return boost::make_transform_iterator(
      base,
      deref{starting_id, refs.data(), data.data(), refs.size()});
  }

  const_iterator begin() const {
    return iterator(refs.begin());
  }

  const_iterator end() const {
    return iterator(refs.end());
  }

  size_t size() const {
    return refs.size();
  }

  size_t dataSize() const {
    return data.size();
  }

  bool defineEach(Define& def) const;

  bool lookupEachType(Lookup& lookup) const;
  bool lookupEachById(Lookup& lookup, bool compare) const;
  bool lookupEachByKey(Lookup& lookup) const;
  bool seekToEach(Lookup& lookup) const;
};

}
}
}
}