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

#include "glean/rts/binary.h"
#include "glean/rts/bytecode/gen/instruction.h"
#include "glean/rts/bytecode/syscall.h"
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
  class Activation final {
   public:
    Activation(const Activation&) = delete;
    Activation(Activation&&) = delete;
    Activation& operator=(const Activation&) = delete;
    Activation& operator=(Activation&&) = delete;

    /// Allocate an activation on the stack and pass it to the supplied function
    /// which needs to 'start' or 'restart' it, initialise its 'args' and then
    /// 'execute' it. Note that the Subroutine must remain alive throughout the
    /// lifetime of the activation.
    template<typename F> static auto with(const Subroutine& sub, SysHandlers syscalls, F&& f) {
      alignas(Activation) unsigned char buf[byteSize(sub)];
      struct Guard {
        Activation *ptr;
        ~Guard() { ptr->~Activation(); }
      };
      Guard guard{new(buf) Activation(sub, syscalls)};
      return std::forward<F>(f)(*guard.ptr);
    }

    /// Set up the activation to start executing the subroutine.
    void start() {
      restart(0, sub.constants.begin(), sub.constants.end());
    }

    /// Set up the activation to restart execution from a previously suspended
    /// state.
    template<typename Iter>
    void restart(uint64_t entry, Iter locals_begin, Iter locals_end) {
      pc = sub.code.data() + entry;
      auto reg = frame() + sub.inputs - sub.outputs;
      for (auto& out : outputs()) {
        *reg++ = reinterpret_cast<uint64_t>(&out);
      }
      std::copy(locals_begin, locals_end, frame() + sub.inputs);
    }

    using arg_insert_iterator = uint64_t *;

    /// Get an inserter for supplying args to the subroutine
    arg_insert_iterator args() {
      return frame();
    }

    using Outputs = folly::Range<binary::Output *>;

    Outputs outputs() {
      return {reinterpret_cast<binary::Output*>(this+1), sub.outputs};
    }

    binary::Output& output(size_t i) {
      return outputs()[i];
    }

    /// Execute the activation. If 'suspended' is true after the call, execution
    /// can be resumed by another call to 'execute'.
    void execute();

    void run(std::initializer_list<uint64_t> arguments) {
      start();
      std::copy(arguments.begin(), arguments.end(), args());
      execute();
      assert(!suspended());
    }

    /// Check if the subroutine has been suspended.
    bool suspended() const {
      return pc != nullptr;
    }

    thrift::internal::SubroutineState toThrift() const;

   private:
    explicit Activation(const Subroutine &sub, SysHandlers syscalls)
      : sub(sub), syscalls(syscalls) {
      for (auto& output : outputs()) {
        new(&output) binary::Output;
      }
    }

    ~Activation() noexcept {
      for (auto& output : outputs()) {
        output.binary::Output::~Output();
      }
    }

    /// We place the outputs and then the frame right after the Activation
    /// object.
    static size_t byteSize(const Subroutine& sub) {
      return sizeof(Activation)
        + sub.outputs * sizeof(binary::Output)
        + sub.frameSize() * sizeof(uint64_t);
    }

    uint64_t *frame() {
      return reinterpret_cast<uint64_t *>(outputs().end());
    }

    const uint64_t *frame() const {
      return const_cast<Activation*>(this)->frame();
    }

    const Subroutine &sub;
    SysHandlers syscalls;

    // null if the activation has finished executing
    const uint64_t * FOLLY_NULLABLE pc;
  };

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
