// Copyright (c) Facebook, Inc. and its affiliates.

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

  /// Execute the subroutine with the given arguments. The number of arguments
  /// is given by 'inputs'. The arguments are copied to their registers before
  /// execution.
  void execute(const uint64_t *args) const;

  /// Restart a subroutine with the given set of regs (must be an
  /// array of size inputs + locals) and initializing pc to the given
  /// offset into the code array.
  void restart(uint64_t *regs, uint64_t offset) const;

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
