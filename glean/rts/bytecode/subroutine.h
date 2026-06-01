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

#include "glean/bytecode/instruction.h"
#include "glean/rts/binary.h"
#include "glean/rts/serialize.h"

namespace facebook {
namespace glean {
namespace rts {

/// A bytecode subroutine which can be executed via 'execute'.
/// Subroutines are immutable and can be shared between multiple
/// activations. The 'Activation' class is used to manage the state
/// of a particular execution of a subroutine.
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

  Subroutine() = default;
  Subroutine(
      std::vector<uint64_t> code0,
      size_t inputs0,
      size_t outputs0,
      size_t locals0,
      std::vector<uint64_t> constants0,
      std::vector<std::string> literals0)
      : code(std::move(code0)),
        inputs(inputs0),
        outputs(outputs0),
        locals(locals0),
        constants(std::move(constants0)),
        literals(std::move(literals0)) {}

  /// Size of the subroutine's frame in words
  size_t frameSize() const {
    return inputs + locals;
  }

  /// Serialize a Subroutine
  static void put(binary::Output& out, const Subroutine& sub);

  /// Deserialize a Subroutine
  static void get(binary::Input& in, Subroutine& s);

  /// A subroutine activation record which can be 'execute'd after supplying
  /// arguments, i.o.w. a class which can be used to execute a Subroutine
  /// and track its state.
  class Activation final {
    /*
     * The `with` function shows how the Activation object is used:
     * `with` allocates a buffer of size `byteSize` on the stack, and
     * constructs an Activation object in it (placement-new). This avoids having
     * to malloc each time. The rest of the buffer is used to store the outputs
     * and the frame. That Activation is then passed to the supplied function,
     * which is expected to `run` it.
     *
     * This means that functions in this class can access the outputs and
     * frame by doing pointer arithmetic from `this`, see `outputs` and `frame`.
     *
     * Here's the layout of the buffer created in `with`:
     *
     * [ Activation object | outputs: binary::Output[outputs] | frame:
     * uint64_t[inputs+locals] ]
     *
     * The frame is an array of 'input' and 'local' registers.
     * - The first 'input' registers are used to provide arguments via `args()`,
     *   while the last `sub.outputs` input registers are binary::Output*
     *   pointers into the `outputs` section of the buffer (see `restart`).
     * - The first 'local' registers receive copies of the subroutine constants
     *   (see `start`) while the rest are used for the subroutine intermediate
     *   results (see `results`).
     */
   public:
    Activation(const Activation&) = delete;
    Activation(Activation&&) = delete;
    Activation& operator=(const Activation&) = delete;
    Activation& operator=(Activation&&) = delete;

    /// Allocate an activation on the stack and pass it to the supplied function
    /// which needs to 'start' or 'restart' it, initialise its 'args' and then
    /// 'execute' it. Note that the Subroutine must remain alive throughout the
    /// lifetime of the activation.
    template <typename F>
    static auto with(const Subroutine& sub, void* context, F&& f) {
      // buf is allocated on the stack to avoid a malloc.
      // It is meant to store the Activation object, followed
      // by the outputs and then the frame, see byteSize.
      alignas(Activation) unsigned char buf[byteSize(sub)];
      // Guard is used to ensure that the stack-allocated Activation
      // is destroyed when the function returns.
      // See the ~Guard() destructor.
      struct Guard {
        Activation* ptr;
        explicit Guard(Activation* p) : ptr(p) {}
        ~Guard() {
          ptr->~Activation();
        }
        Guard(const Guard&) = delete;
        Guard& operator=(const Guard&) = delete;
        Guard(Guard&&) = delete;
        Guard& operator=(Guard&&) = delete;
      };
      // Construct the Activation in the buffer, i.e. on the stack.
      Guard guard{new (buf) Activation(sub, context)};
      return std::forward<F>(f)(*guard.ptr);
    }

    /// Set up the activation to start executing the subroutine.
    /// Constants are copied into the first locals registers.
    void start() {
      restart(0, sub.constants.begin(), sub.constants.end());
    }

    /// Set up the activation to restart execution from a previously suspended
    /// state.
    template <typename Iter>
    void restart(uint64_t entry, Iter locals_begin, Iter locals_end) {
      pc = sub.code.data() + entry;
      // The last 'sub.outputs' registers of the 'inputs' registers are
      // binary::Output* pointers into the `outputs()` section.
      auto reg = frame() + sub.inputs - sub.outputs;
      for (auto& out : outputs()) {
        *reg++ = reinterpret_cast<uint64_t>(&out);
      }
      std::copy(locals_begin, locals_end, frame() + sub.inputs);
    }

    using arg_insert_iterator = uint64_t*;

    /// Get an inserter for supplying args to the subroutine
    arg_insert_iterator args() {
      return frame();
    }

    using Outputs = folly::Range<binary::Output*>;

    Outputs outputs() {
      // The outputs are stored immediately after the Activation object as
      // binary::Output objects.
      return {reinterpret_cast<binary::Output*>(this + 1), sub.outputs};
    }

    binary::Output& output(size_t i) {
      return outputs()[i];
    }

    /// Registers in which a subroutine returns its intermediate results.
    /// These are basically the local registers that are not already used
    /// for constants.
    folly::Range<const uint64_t*> results() const {
      return {
          frame() + sub.inputs + sub.constants.size(),
          frame() + sub.frameSize()};
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

    /// Serialize an Activation.
    void put(binary::Output& out) const;

    static void put(binary::Output& out, const Subroutine::Activation& act) {
      act.put(out);
    }

    /// Deserialize an Activation, producing a Subroutine and
    /// a State. To continue execution, do something like:
    ///
    ///    auto [sub, state] = get(input);
    ///    Activation::with(sub, context, [](Activation& act) {
    ///       act.resume(std::move(state));
    ///       ...
    ///       act.execute();
    ///    }
    struct State {
      uint64_t pc;
      std::vector<uint64_t> locals;
      std::vector<binary::Output> outputs;
    };
    static std::pair<Subroutine, State> get(binary::Input& in);

    /// Set up the Activation from a previously serialized State
    void resume(State s) {
      restart(s.pc, s.locals.begin(), s.locals.end());
      auto buf = s.outputs.begin();
      for (auto& out : outputs()) {
        out = std::move(*buf);
        buf++;
      }
    }

   private:
    explicit Activation(const Subroutine& sub, void* context)
        : sub(sub), context(context) {
      for (auto& output : outputs()) {
        new (&output) binary::Output;
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
      return sizeof(Activation) + sub.outputs * sizeof(binary::Output) +
          sub.frameSize() * sizeof(uint64_t);
    }

    uint64_t* frame() {
      return reinterpret_cast<uint64_t*>(outputs().end());
    }

    const uint64_t* frame() const {
      return const_cast<Activation*>(this)->frame();
    }

    const Subroutine& sub;
    void* context;

    // null if the activation has finished executing
    const uint64_t* FOLLY_NULLABLE pc;
  };

  bool operator==(const Subroutine& other) const;
  bool operator!=(const Subroutine& other) const {
    return !(*this == other);
  }

  /// Return the size of this bytecode routine in bytes, including the
  /// code, literals and constants.
  size_t size() const;
};

} // namespace rts
} // namespace glean
} // namespace facebook
