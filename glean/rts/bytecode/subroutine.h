/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <string>
#include <vector>

#include "glean/rts/bytecode/gen/instruction.h"
#include "glean/if/gen-cpp2/internal_types.h"

namespace facebook {
namespace glean {
namespace rts {

/// A bytecode subroutine which can be execute via 'execute'.
struct Subroutine {

  /// Instructions
  std::vector<uint64_t> code;

  /// Number of input words (available in registers [0,inputs-1])
  size_t inputs;

  /// Number of input words that are binary::Output*. This is not used
  /// by the bytecode evaluator itself, but is useful for code that
  /// invokes it to know how many binary::Output objects to allocate.
  size_t outputs;

  /// Number of local registers (in [inputs,inputs+locals-1])
  size_t locals;

  std::vector<uint64_t> constants;

  /// Array of literal which can be accessed via their index from some
  /// instructions.
  std::vector<std::string> literals;

  Subroutine() = delete;
  Subroutine(const std::vector<uint64_t>& code0,
                  size_t inputs0,
                  size_t outputs0,
                  size_t locals0,
                  const std::vector<uint64_t>& constants0,
                  const std::vector<std::string>& literals0):
          code(code0),
          inputs(inputs0),
          outputs(outputs0),
          locals(locals0),
          constants(constants0),
          literals(literals0){}

  /// Size of the subroutine's frame in words
  size_t frameSize() const {
    return inputs + locals;
  }

  /// A subroutine activation record which can be 'execute'd after supplying
  /// arguments. The activation requires a pointer to a preallocated frame but
  /// doesn't own it. This is mostly because we want to be able to avoid having
  /// to 'malloc' the frame for each execution.
  struct Activation final {
    const Subroutine *sub;
    uint64_t *frame;
    uint64_t entry;

    /// Initialise the activation. Both `sub` and `frame` have to remain alive
    /// throughout the execution of activation.
    template<typename Iter>
    Activation(
        const Subroutine *sub,
        uint64_t* frame,
        uint64_t entry,
        Iter locals_begin,
        Iter locals_end) : sub(sub), frame(frame), entry(entry) {
      std::copy(locals_begin, locals_end, frame + sub->inputs);
    }

    using arg_insert_iterator = uint64_t *;

    /// Get an inserter for supplying args to the subroutine
    arg_insert_iterator args() {
      return frame;
    }

    void execute();
  };

  /// Create an activation record for the subroutine.
  ///
  /// @param frame Preallocated (but not initialised) frame of at least
  /// 'frameSize' words.
  Activation activate(uint64_t *frame) const {
    return Activation(this, frame, 0, constants.begin(), constants.end());
  }

  /// Restore an activation record for the subroutine.
  ///
  /// @param frame Preallocated (but not initialised) frame of at least
  /// 'frameSize' words.
  /// @param entry Offset into the subroutine's code.
  /// @param locals_begin Start of local registers to restore.
  /// @param locals_end End of local registers to restore.
  template<typename Iter>
  Activation restart(uint64_t *frame, uint64_t entry, Iter locals_begin, Iter locals_end) const {
    return Activation(this, frame, entry, locals_begin, locals_end);
  }

  /// Execute the subroutine with the given arguments.
  void execute(std::initializer_list<uint64_t> args) const {
    uint64_t frame[frameSize()];
    auto activation = activate(frame);
    std::copy(args.begin(), args.end(), activation.args());
    activation.execute();
  }

  bool operator==(const Subroutine& other) const;
  bool operator!=(const Subroutine& other) const {
    return !(*this == other);
  }

  /// Return the size of this bytecode routine in bytes, including the
  /// code, literals and constants.
  size_t size() const;

  // Deserialize a Subroutine from thrift::internal::Subroutine
  static std::shared_ptr<Subroutine> fromThrift(
      const thrift::internal::Subroutine &ser);

  // Serialize a Subroutine as thrift::internal::Subroutine
  static thrift::internal::Subroutine toThrift(const Subroutine& sub);
};

}
}
}
