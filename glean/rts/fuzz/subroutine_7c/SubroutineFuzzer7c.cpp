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
//
// Improvements:
//   - Added direct construction path (coin flip) to exercise the
//     outputs loop body in restart/resume (subroutine.h:151).
//     The deserialization path rarely produces non-empty outputs
//     because the outputs field is last in the serialization order
//     and earlier fields consume all fuzzer bytes.
// =============================================================

#include "glean/rts/binary.h"
#include "glean/rts/bytecode/subroutine.h"

#include "security/lionhead/utils/lib_ftest/ftest.h"

using namespace facebook::glean::binary;
using namespace facebook::glean::rts;

FUZZ(SubroutineFuzzer7c, FuzzActivationWith) {
  if (f.coin("direct_path")) {
    // Direct construction path: exercise the outputs loop in
    // restart/resume by constructing a Subroutine with fuzzed
    // counts. Uses u8 for counts to keep the VLA allocation
    // within stack bounds while still allowing the fuzzer to
    // explore the outputs loop body (subroutine.h:151) and the
    // resume outputs copy loop (subroutine.h:227-229).
    auto num_outputs = f.u8("outputs");
    auto num_inputs = f.u8("inputs");
    auto num_locals = f.u8("locals");
    auto pc_val = f.u64("pc");

    Subroutine sub(
        std::vector<uint64_t>{},
        static_cast<size_t>(num_inputs),
        static_cast<size_t>(num_outputs),
        static_cast<size_t>(num_locals),
        std::vector<uint64_t>(),
        std::vector<std::string>());

    std::vector<uint64_t> locals_vec(num_locals);
    std::vector<Output> outputs_vec(num_outputs);

    try {
      Subroutine::Activation::State state{
          pc_val, std::move(locals_vec), std::move(outputs_vec)};
      Subroutine::Activation::with(
          sub, nullptr, [&](Subroutine::Activation& act) {
            act.resume(std::move(state));
          });
    } catch (...) {
    }
  } else {
    // Deserialization path (original): pass raw fuzzed bytes through
    // Activation::get to exercise the full deserialization + VLA
    // overflow path. The deserialized inputs/outputs/locals counts
    // are unbounded, which is the primary overflow vector.
    auto data = f.all_remaining_bytes();
    Input input(folly::ByteRange(data.data(), data.size()));

    try {
      auto [sub, state] = Subroutine::Activation::get(input);
      Subroutine::Activation::with(
          sub, nullptr, [&](Subroutine::Activation& act) {
            act.resume(std::move(state));
          });
    } catch (...) {
    }
  }
}
