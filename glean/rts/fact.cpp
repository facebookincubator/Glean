/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/fact.h"

#include <folly/String.h>
#include <sstream>

namespace facebook {
namespace glean {
namespace rts {

void Fact::serialize(binary::Output& output, Pid type, Fact::Clause clause) {
  output.packed(type);
  output.packed(clause.key_size);
  output.packed(clause.value_size);
  output.put(clause.bytes());
}

void Fact::deserialize(binary::Input& input, Pid& type, Fact::Clause& clause) {
  type = input.packed<Pid>();
  const auto key_size = input.packed<uint32_t>();
  const auto value_size = input.packed<uint32_t>();
  const auto data = input.bytes(key_size + value_size).data();
  clause = {data, key_size, value_size};
}

std::string Fact::dump() const {
  std::ostringstream os;

  auto bytes = [](std::ostream& os, folly::ByteRange bs) {
    if (std::all_of(
            bs.begin(), bs.end(), [](auto c) { return c >= 32 && c < 127; })) {
      os << '"' << std::string(bs.begin(), bs.end()) << '"';
    } else {
      os << folly::hexlify(bs);
    }
  };

  os << '{' << id().toWord() << "} {" << type().toWord() << "} ";
  bytes(os, key());
  os << ' ';
  bytes(os, value());

  return os.str();
}

} // namespace rts
} // namespace glean
} // namespace facebook
