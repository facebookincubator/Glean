/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// =============================================================
// Harness Intention:
//   Fuzzes Subroutine::Activation::with to trigger integer overflow
//   in byteSize() computation. When attacker-controlled outputs/inputs/
//   locals counts overflow size_t in the multiply, the stack VLA
//   buf[byteSize(sub)] is undersized. The Activation constructor and
//   restart() then write past the end of the buffer (stack OOB write).
//
//   Mirrors the production ingress path:
//     query.cpp:769 -> Activation::get(in) -> Activation::with(sub, ...)
// =============================================================

#include "glean/rts/binary.h"
#include "glean/rts/bytecode/subroutine.h"

#include "security/lionhead/utils/lib_ftest/ftest.h"

using namespace facebook::glean::binary;
using namespace facebook::glean::rts;

FUZZ(SubroutineFuzzer7c, FuzzActivationWith) {
  auto data = f.all_remaining_bytes();
  Input input(folly::ByteRange(data.data(), data.size()));

  try {
    // Deserialize a (Subroutine, State) pair from fuzzed bytes.
    // The deserialized inputs/outputs/locals counts are unbounded,
    // which is the primary overflow vector.
    auto [sub, state] = Subroutine::Activation::get(input);

    // Drive the deserialized subroutine through Activation::with so
    // the byteSize() VLA is allocated and the constructor + resume()
    // write into it. If byteSize overflows, the buffer is undersized
    // and the writes go past the end (stack OOB).
    Subroutine::Activation::with(
        sub, nullptr, [&](Subroutine::Activation& act) {
          act.resume(std::move(state));
        });
  } catch (...) {
  }
}
