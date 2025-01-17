/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <glog/logging.h>

#include "glean/rts/binary.h"
#include "glean/rts/bytecode/subroutine.h"
#include "glean/rts/bytecode/syscall.h"
#include "glean/rts/prim.h"
#include "glean/rts/serialize.h"
#include "glean/rts/string.h"

namespace facebook {
namespace glean {
namespace rts {

namespace {

struct Eval {
  const std::string* literals;
  const uint64_t* code;
  const uint64_t* pc;
  void* context;
  uint64_t* frame;

#include "glean/bytecode/evaluate.h"

  FOLLY_ALWAYS_INLINE void execute(InputNat a) {
    binary::Input input{*a.begin, a.end};
    a.dst << input.packed<uint64_t>();
    a.begin << input.data();
  }

  FOLLY_ALWAYS_INLINE void execute(InputBytes a) {
    binary::Input input{*a.begin, a.end};
    input.bytes(a.size);
    a.begin << input.data();
  }

  FOLLY_ALWAYS_INLINE void execute(InputSkipUntrustedString a) {
    binary::Input input{*a.begin, a.end};
    input.skipUntrustedString();
    a.begin << input.data();
  }

  FOLLY_ALWAYS_INLINE void execute(InputShiftLit a) {
    binary::Input input{*a.begin, a.end};
    a.match << input.shift(binary::byteRange(*a.lit));
    a.begin << input.data();
  }

  FOLLY_ALWAYS_INLINE void execute(InputShiftBytes a) {
    binary::Input input{*a.begin, a.end};
    a.match << input.shift(folly::ByteRange(a.ptr, a.ptrend));
    a.begin << input.data();
  }

  FOLLY_ALWAYS_INLINE void execute(InputSkipNat a) {
    binary::Input input{*a.begin, a.end};
    input.packed<uint64_t>();
    a.begin << input.data();
  }

  FOLLY_ALWAYS_INLINE void execute(InputSkipTrustedString a) {
    binary::Input input{*a.begin, a.end};
    input.skipTrustedString();
    a.begin << input.data();
  }

  FOLLY_ALWAYS_INLINE void execute(OutputStringToLower a) {
    toLowerTrustedString({a.begin, a.end}, *a.dst);
  }

  FOLLY_ALWAYS_INLINE void execute(OutputRelToAbsByteSpans a) {
    relToAbsByteSpans({a.begin, a.end}, *a.dst);
  }

  FOLLY_ALWAYS_INLINE void execute(OutputUnpackByteSpans a) {
    unpackByteSpans({a.begin, a.end}, *a.dst);
  }

  FOLLY_ALWAYS_INLINE void execute(OutputStringReverse a) {
    a.dst->reverseString({a.begin, a.end});
  }

  FOLLY_ALWAYS_INLINE void execute(ResetOutput a) {
    *a.output = binary::Output();
  }

  FOLLY_ALWAYS_INLINE void execute(OutputByte a) {
    a.output->fixed<unsigned char>(static_cast<unsigned char>(a.src));
  }

  FOLLY_ALWAYS_INLINE void execute(OutputByteImm a) {
    a.output->fixed<unsigned char>(static_cast<unsigned char>(a.src));
  }

  FOLLY_ALWAYS_INLINE void execute(OutputNat a) {
    a.output->packed(a.src);
  }

  FOLLY_ALWAYS_INLINE void execute(OutputNatImm a) {
    a.output->packed(a.src);
  }

  FOLLY_ALWAYS_INLINE void execute(OutputBytes a) {
    a.output->bytes(a.ptr, a.end - a.ptr);
  }

  FOLLY_ALWAYS_INLINE void execute(GetOutput a) {
    a.ptr << a.output->bytes().data();
    a.end << a.output->bytes().end();
  }

  FOLLY_ALWAYS_INLINE void execute(GetOutputSize a) {
    a.dst << a.output->size();
  }

  FOLLY_ALWAYS_INLINE void execute(LoadConst a) {
    a.dst << a.imm;
  }

  FOLLY_ALWAYS_INLINE void execute(LoadLiteral a) {
    a.ptr << reinterpret_cast<const unsigned char*>(a.lit->data());
    a.end << reinterpret_cast<const unsigned char*>(
        a.lit->data() + a.lit->size());
  }

  FOLLY_ALWAYS_INLINE void execute(Move a) {
    a.dst << a.src;
  }

  FOLLY_ALWAYS_INLINE void execute(SubConst a) {
    a.dst << *a.dst - a.imm;
  }

  FOLLY_ALWAYS_INLINE void execute(AddConst a) {
    a.dst << *a.dst + a.imm;
  }

  FOLLY_ALWAYS_INLINE void execute(Sub a) {
    a.dst << *a.dst - a.src;
  }

  FOLLY_ALWAYS_INLINE void execute(Add a) {
    a.dst << *a.dst + a.src;
  }

  FOLLY_ALWAYS_INLINE void execute(PtrDiff a) {
    a.dst << a.src2 - a.src1;
  }

  FOLLY_ALWAYS_INLINE void execute(LoadLabel a) {
    a.dst << static_cast<uint64_t>(pc - code + std::ptrdiff_t(a.lbl));
    // turn relative into absolute offset
  }

  FOLLY_ALWAYS_INLINE void execute(Jump a) {
    pc += std::ptrdiff_t(a.tgt);
  }

  FOLLY_ALWAYS_INLINE void execute(JumpReg a) {
    pc = code + a.tgt;
  }

  FOLLY_ALWAYS_INLINE void execute(JumpIf0 a) {
    if (a.reg == 0) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(JumpIfNot0 a) {
    if (a.reg != 0) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(JumpIfEq a) {
    if (a.reg1 == a.reg2) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(JumpIfNe a) {
    if (a.reg1 != a.reg2) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(JumpIfGt a) {
    if (a.reg1 > a.reg2) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(JumpIfGe a) {
    if (a.reg1 >= a.reg2) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(JumpIfLt a) {
    if (a.reg1 < a.reg2) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(JumpIfLe a) {
    if (a.reg1 <= a.reg2) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(DecrAndJumpIfNot0 a) {
    a.reg << *a.reg - 1;
    if (*a.reg != 0) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(DecrAndJumpIf0 a) {
    a.reg << *a.reg - 1;
    if (*a.reg == 0) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_0_1 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_0_2 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_1_1 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_1_0 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_2_0 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_3_0 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_4_0 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_2_1 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_2_2 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_2_5 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_3_1 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_5_0 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_5_1 a) {
    (*a.fun)(context, frame, a.args, a.args_arity);
  }

  FOLLY_ALWAYS_INLINE void execute(Select a) {
    if (a.sel < a.tgts_size) {
      pc += std::ptrdiff_t(a.tgts[a.sel]);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(Raise a) {
    rts::error(*a.msg);
  }

  FOLLY_ALWAYS_INLINE void execute(Trace a) {
    LOG(INFO) << *a.msg;
  }

  FOLLY_ALWAYS_INLINE void execute(TraceReg a) {
    LOG(INFO) << *a.msg << ": " << folly::to<std::string>(a.reg);
  }

  FOLLY_ALWAYS_INLINE const uint64_t* execute(Suspend a) {
    return pc + std::ptrdiff_t(a.cont);
  }

  FOLLY_ALWAYS_INLINE const uint64_t* FOLLY_NULLABLE execute(Ret) {
    return nullptr;
  }
};

} // namespace

#define USE_SWITCH 1

void Subroutine::Activation::execute() {
  pc = Eval{sub.literals.data(), sub.code.data(), pc, context, frame()}.
#if USE_SWITCH
       evalSwitch();
#else
       evalIndirect();
#endif
}

bool Subroutine::operator==(const Subroutine& other) const {
  return this == &other ||
      (code == other.code && inputs == other.inputs &&
       outputs == other.outputs && locals == other.locals &&
       constants == other.constants && literals == other.literals);
}

size_t Subroutine::size() const {
  size_t litsz = 0;
  for (auto& lit : literals) {
    litsz += lit.size();
  }
  return litsz + sizeof(this) + code.size() * sizeof(uint64_t) +
      constants.size() * sizeof(uint64_t);
}

namespace {
template <typename T, typename U>
std::vector<T> copy_as(const std::vector<U>& xs) {
  std::vector<T> ys;
  ys.reserve(xs.size());
  for (const auto& x : xs) {
    ys.push_back(static_cast<T>(x));
  }
  return ys;
}
} // namespace

void Subroutine::put(binary::Output& out, const Subroutine& sub) {
  serialize::put(out, sub.code, serialize::AsBytes{});
  serialize::put(out, sub.inputs);
  serialize::put(out, sub.outputs);
  serialize::put(out, sub.locals);
  serialize::put(out, sub.constants);
  serialize::put(out, sub.literals);
}

void Subroutine::get(binary::Input& in, Subroutine& sub) {
  serialize::get(in, sub.code, serialize::AsBytes{});
  serialize::get(in, sub.inputs);
  serialize::get(in, sub.outputs);
  serialize::get(in, sub.locals);
  serialize::get(in, sub.constants);
  serialize::get(in, sub.literals);
}

/// Serialize an Activation. The Activation is notionally
///   Subroutine + current PC + locals + outputs
/// but we don't use the standard Subroutine serialization
/// because we can do it slightly more efficiently by
/// serializing just the bits we need.
void Subroutine::Activation::put(binary::Output& out) const {
  serialize::put(out, sub.code, serialize::AsBytes{});
  serialize::put(out, (uint64_t)(pc - sub.code.data()));
  serialize::put(out, sub.literals);
  const folly::Range<const uint64_t*> locals{frame() + sub.inputs, sub.locals};
  serialize::put(out, locals);
  serialize::put(out, sub.inputs);
  const folly::Range<const binary::Output*> outputs{
      reinterpret_cast<const binary::Output*>(this + 1), sub.outputs};
  serialize::put(out, outputs);
}

std::pair<Subroutine, Subroutine::Activation::State>
Subroutine::Activation::get(binary::Input& in) {
  uint64_t pc;
  std::vector<uint64_t> code;
  size_t inputs;
  std::vector<uint64_t> locals;
  std::vector<std::string> literals;
  std::vector<binary::Output> outputs;

  serialize::get(in, code, serialize::AsBytes{});
  serialize::get(in, pc);
  serialize::get(in, literals);
  serialize::get(in, locals);
  serialize::get(in, inputs);
  serialize::get(in, outputs);

  auto sub = Subroutine(
      std::move(code),
      inputs,
      outputs.size(),
      locals.size(),
      std::vector<uint64_t>(), // no constants, they're already on the stack
      std::move(literals));

  Subroutine::Activation::State state{
      pc, std::move(locals), std::move(outputs)};
  return {std::move(sub), std::move(state)};
}

} // namespace rts
} // namespace glean
} // namespace facebook
