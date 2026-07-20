/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// ## Threat model
//
// Adversarial  Glean query whose compiled `Subroutine` (packed uint64
// instruction stream + constants + literals) is executed by the Glean RTS
// via `Activation::execute()`; production caller is the query-restart
// continuation path exercised by the parent
// `GleanActivationRestartFuzzer23`. Trust boundary: post-auth service-side
// (Glean RTS query engine consuming client-issued queries). Impact:
// memory-safety (wild-addr-read in `binary::Output::bytes()` and
// `basic_string::_M_data`) + DoS (uncaught-exception,
// allocation-size-too-big). Precedent: T273591336, T273683144, T273848440,
// T273848577.

// Derivative harness for `GleanActivationRestartFuzzer23` — narrows the
// fuzz surface from "restart an existing continuation blob" down to
// "execute an arbitrary Subroutine directly".
//
// Motivation
// ==========
// `GleanActivationRestartFuzzer23` produced FOUR distinct bugs, all in
// the RTS execution engine:
//   - T273591336  wild-addr-read in `std::__cxx11::basic_string::_M_data`
//   - T273683144  uncaught-exception in `glean::rts::raiseError` (invalid
//   opcode)
//   - T273848440  wild-addr-read in `glean::binary::Output::bytes()`
//   - T273848577  allocation-size-too-big in `folly::checkedRealloc`
//
// All four are in the bytecode-execution / binary::Output path, not in
// query-restart glue. The parent harness has to first construct a
// serialized restart continuation, then deserialize it, then execute —
// mutation loses many iterations on the two front stages before
// reaching the sink.
//
// Shape
// =====
// This derivative shortcuts everything before `Activation::execute()`:
// each iteration reads FDP bytes and constructs a `Subroutine` DIRECTLY
// with fuzzer-controlled `code` (packed uint64 instruction stream),
// `constants`, `literals`, and `inputs / outputs / locals` counts. It
// then wraps the whole `execute()` in a try/catch to swallow
// well-formed Glean errors while letting real memory-safety violations
// terminate the process (as expected under ASan / MSan).
//
// All FDP-derived sizes are strictly bounded so `Activation::with`'s
// stack buffer (`alignas(Activation) unsigned char buf[byteSize(sub)]`)
// stays under the fuzzer's stack limit. Without these bounds, a single
// input could ask for terabytes of stack and hang the fuzzer before
// reaching the interesting execute() paths.

#include "security/lionhead/utils/lib_ftest/ftest.h"

#include "glean/rts/binary.h"
#include "glean/rts/bytecode/subroutine.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <string>
#include <vector>

using namespace facebook::security::lionhead::fdp;

namespace {

// -----------------------------------------------------------------------
// Bounds. These keep byteSize(sub) — the stack buffer that
// `Activation::with` alloca's — well under a reasonable per-iteration
// stack budget. byteSize expands to:
//   sizeof(Activation) + outputs*sizeof(binary::Output) + (inputs+locals)*8
// With outputs<=8, binary::Output ~= 64 bytes → 512 B for outputs, and
// inputs+locals<=64 → 512 B for the frame. Plus ~128 B for Activation
// itself. Total well under 4 KiB, comfortably below the 128 KiB
// per-iteration stack budget Lionhead runs with.
// -----------------------------------------------------------------------

constexpr size_t kMaxCodeWords = 512;
constexpr size_t kMaxConstants = 32;
constexpr size_t kMaxLiterals = 16;
constexpr size_t kMaxLiteralLen = 128;
constexpr uint32_t kMaxInputs = 32;
constexpr uint32_t kMaxOutputs = 8;
constexpr uint32_t kMaxLocals = 32;
constexpr uint32_t kMaxCallerArgs = 24; // inputs - outputs

} // namespace

// Fuzz the bytecode execution engine directly by synthesizing a
// Subroutine + running it. The RTS is expected to raise Glean-specific
// exceptions on malformed programs; we catch those so the fuzzer stays
// alive across iterations. Any memory-safety violation (heap OOB,
// use-after-free, wild-addr read via `binary::Output::bytes`, etc.)
// escapes the catch and is caught by ASan / MSan — those are the real
// findings this harness exists to surface.
FUZZ(GleanRtsEvalBytecode, fuzzer) {
  // ---------------------------------------------------------------------
  // Shape parameters — bounded so byteSize(sub) stays small.
  // ---------------------------------------------------------------------
  const uint32_t outputs = f.u32_range("outputs", 0, kMaxOutputs);
  // inputs must be >= outputs (subroutine.h L149: `frame() + sub.inputs
  // - sub.outputs` underflows for `inputs < outputs`).
  const uint32_t inputs =
      outputs + f.u32_range("caller_args", 0, kMaxCallerArgs);
  const uint32_t locals = f.u32_range("locals", 0, kMaxLocals);

  // ---------------------------------------------------------------------
  // Constants (uint64 words the subroutine copies into its frame at
  // start()). Bounded so `Subroutine::constants.size() > locals` doesn't
  // overflow the frame copy at `restart(entry, constants_begin,
  // constants_end)`.
  // ---------------------------------------------------------------------
  const uint32_t num_constants =
      f.u32_range("num_constants", 0, std::min(kMaxConstants, size_t{locals}));
  std::vector<uint64_t> constants;
  constants.reserve(num_constants);
  for (uint32_t i = 0; i < num_constants; ++i) {
    constants.push_back(f.u64("const"));
  }

  // ---------------------------------------------------------------------
  // Literals (referenced by their index from string-bearing opcodes).
  // Every literal is a fuzz-controlled byte sequence.
  // ---------------------------------------------------------------------
  const uint32_t num_literals = f.u32_range("num_literals", 0, kMaxLiterals);
  std::vector<std::string> literals;
  literals.reserve(num_literals);
  for (uint32_t i = 0; i < num_literals; ++i) {
    auto lit_bytes = f.bytes_max_length("lit", kMaxLiteralLen);
    literals.emplace_back(
        reinterpret_cast<const char*>(lit_bytes.data()), lit_bytes.size());
  }

  // ---------------------------------------------------------------------
  // Code — packed uint64 instruction stream. We consume raw bytes and
  // treat them as a little-endian uint64 array. Random 64-bit words
  // fed into the dispatcher exercise the opcode-decoder switch AND the
  // per-opcode operand-interpretation code (which is where the four
  // historical bugs sit). Bounded at kMaxCodeWords so per-iteration
  // execution time stays sub-second.
  // ---------------------------------------------------------------------
  const uint32_t num_code_words =
      f.u32_range("num_code_words", 1, kMaxCodeWords);
  const size_t code_bytes_needed = num_code_words * sizeof(uint64_t);
  auto raw_code = f.bytes_max_length("code", code_bytes_needed);
  if (raw_code.size() < sizeof(uint64_t)) {
    return;
  }
  const size_t whole_words = raw_code.size() / sizeof(uint64_t);
  if (whole_words == 0) {
    return;
  }
  std::vector<uint64_t> code(whole_words);
  std::memcpy(code.data(), raw_code.data(), whole_words * sizeof(uint64_t));

  // Caller-supplied args match the `inputs - outputs` slots per
  // subroutine.h L149. Zero-fill (execution semantics don't need
  // interesting arg content for the bug pattern — the interesting
  // fuzz input is the code stream itself).
  const uint32_t num_caller_args = inputs - outputs;
  std::vector<uint64_t> args(num_caller_args, 0);

  // ---------------------------------------------------------------------
  // Build + execute. `Activation::with` stack-allocates a buffer sized
  // by byteSize(sub); the bounds above keep that buffer small.
  // ---------------------------------------------------------------------
  facebook::glean::rts::Subroutine sub(
      std::move(code),
      inputs,
      outputs,
      locals,
      std::move(constants),
      std::move(literals));

  try {
    facebook::glean::rts::Subroutine::Activation::with(
        sub, /*context=*/nullptr, [&](auto& act) {
          act.start();
          std::copy(args.begin(), args.end(), act.args());
          act.execute();
        });
  } catch (const std::exception&) {
    // Glean throws on malformed opcodes / invalid operands. Expected.
  } catch (...) {
    // Non-std exception types (Glean has its own hierarchy in some
    // spots). Same handling — expected under fuzz input.
  }
}
