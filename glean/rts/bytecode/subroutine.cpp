#include "glean/rts/binary.h"
#include "glean/rts/bytecode/subroutine.h"
#include "glean/rts/id.h"
#include "glean/rts/string.h"
#include "glean/rts/prim.h"

namespace facebook {
namespace glean {
namespace rts {

namespace {

struct Eval {
  const std::string *literals;
  const uint64_t *code;
  const uint64_t *pc;
  uint64_t *frame;

#include "glean/rts/bytecode/gen/evaluate.h"

  FOLLY_ALWAYS_INLINE void execute(InputNat a) {
    binary::Input input { *a.begin, a.end };
    *a.dst = input.packed<uint64_t>();
    *a.begin = reinterpret_cast<void *>(
        const_cast<unsigned char*>(input.data()));
  }

  FOLLY_ALWAYS_INLINE void execute(InputBytes a) {
    binary::Input input { *a.begin, a.end };
    input.bytes(a.size);
    *a.begin = reinterpret_cast<void *>(
        const_cast<unsigned char*>(input.data()));
  }

  FOLLY_ALWAYS_INLINE void execute(InputSkipUntrustedString a) {
    binary::Input input { *a.begin, a.end };
    input.skipUntrustedString();
    *a.begin = reinterpret_cast<void *>(
        const_cast<unsigned char*>(input.data()));
  }

  FOLLY_ALWAYS_INLINE void execute(InputShiftLit a) {
    binary::Input input { *a.begin, a.end };
    *a.match = input.shift(binary::byteRange(*a.lit));
    *a.begin = reinterpret_cast<void *>(
        const_cast<unsigned char*>(input.data()));
  }

  FOLLY_ALWAYS_INLINE void execute(InputShiftBytes a) {
    binary::Input input { *a.begin, a.end };
    *a.match = input.shift(
        folly::ByteRange(reinterpret_cast<const unsigned char*>(a.ptr),
                         reinterpret_cast<const unsigned char*>(a.ptrend)));
    *a.begin = reinterpret_cast<void *>(
        const_cast<unsigned char*>(input.data()));
  }

  FOLLY_ALWAYS_INLINE void execute(InputSkipNat a) {
    binary::Input input { *a.begin, a.end };
    input.packed<uint64_t>();
    *a.begin = reinterpret_cast<void *>(
        const_cast<unsigned char*>(input.data()));
  }

  FOLLY_ALWAYS_INLINE void execute(InputSkipTrustedString a) {
    binary::Input input { *a.begin, a.end };
    input.skipTrustedString();
    *a.begin = reinterpret_cast<void *>(
        const_cast<unsigned char*>(input.data()));
  }

  FOLLY_ALWAYS_INLINE void execute(OutputStringToLower a) {
    folly::ByteRange input {
      reinterpret_cast<const unsigned char*>(a.begin),
      reinterpret_cast<const unsigned char*>(a.end) };
    toLowerTrustedString(input, *a.dst);
  }


  FOLLY_ALWAYS_INLINE void execute(OutputRelToAbsByteSpans a) {
    folly::ByteRange input {
      reinterpret_cast<const uint8_t*>(a.begin),
      reinterpret_cast<const uint8_t*>(a.end) };
    relToAbsByteSpans(input, *a.dst);
  }

  FOLLY_ALWAYS_INLINE void execute(ResetOutput a) {
    *a.output = binary::Output();
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
    a.output->bytes(
        reinterpret_cast<const void *>(a.ptr),
        reinterpret_cast<uintptr_t>(a.end) -
        reinterpret_cast<uintptr_t>(a.ptr));
  }

  FOLLY_ALWAYS_INLINE void execute(GetOutput a) {
    *a.ptr = reinterpret_cast<void *>(
        const_cast<unsigned char*>(a.output->bytes().data()));
    *a.end = reinterpret_cast<void *>(
        const_cast<unsigned char*>(a.output->bytes().end()));
  }

  FOLLY_ALWAYS_INLINE void execute(GetOutputSize a) {
    *a.dst = a.output->size();
  }

  FOLLY_ALWAYS_INLINE void execute(LoadConst a) {
    *a.dst = a.imm;
  }

  FOLLY_ALWAYS_INLINE void execute(LoadLiteral a) {
    *a.ptr = reinterpret_cast<uint64_t *>(const_cast<char *>(a.lit->data()));
    *a.end = reinterpret_cast<uint64_t *>(
        const_cast<char *>(a.lit->data() + a.lit->size()));
  }

  FOLLY_ALWAYS_INLINE void execute(LoadReg a) {
    *a.dst = a.src;
  }

  FOLLY_ALWAYS_INLINE void execute(SubConst a) {
    *a.dst -= a.imm;
  }

  FOLLY_ALWAYS_INLINE void execute(AddConst a) {
    *a.dst += a.imm;
  }

  FOLLY_ALWAYS_INLINE void execute(Sub a) {
    *a.dst -= a.src;
  }

  FOLLY_ALWAYS_INLINE void execute(Add a) {
    *a.dst += a.src;
  }

  FOLLY_ALWAYS_INLINE void execute(PtrDiff a) {
    *a.dst = reinterpret_cast<uint64_t>(a.src2) -
      reinterpret_cast<uint64_t>(a.src1);
  }

  FOLLY_ALWAYS_INLINE void execute(LoadLabel a) {
    *a.dst = static_cast<uint64_t>(pc - code + std::ptrdiff_t(a.lbl));
      // turn relative into absolute offset
  }

  FOLLY_ALWAYS_INLINE void execute(Jump a) {
    pc += std::ptrdiff_t(a.tgt);
  }

  FOLLY_ALWAYS_INLINE void execute(JumpReg a) {
    pc = reinterpret_cast<const uint64_t*>(code + a.tgt);
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
    --*a.reg;
    if (*a.reg != 0) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(DecrAndJumpIf0 a) {
    --*a.reg;
    if (*a.reg == 0) {
      pc += std::ptrdiff_t(a.tgt);
    }
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_0_1 a) {
    *a.dst = (*a.fun)();
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_0_2 a) {
    *a.dst1 = (*a.fun)(a.dst2);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_1_1 a) {
    *a.dst = (*a.fun)(a.src);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_1_0 a) {
    (*a.fun)(a.src);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_2_0 a) {
    (*a.fun)(a.src1, a.src2);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_3_0 a) {
    (*a.fun)(a.src1, a.src2, a.src3);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_4_0 a) {
    (*a.fun)(a.src1, a.src2, a.src3, a.src4);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_2_1 a) {
    *a.dst = (*a.fun)(a.src1, a.src2);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_2_2 a) {
    *a.dst1 = (*a.fun)(a.src1, a.src2, a.dst2);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_2_5 a) {
    *a.dst1 = (*a.fun)(a.src1, a.src2, a.dst2, a.dst3, a.dst4, a.dst5);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_3_1 a) {
    *a.dst = (*a.fun)(a.src1, a.src2, a.src3);
  }

  FOLLY_ALWAYS_INLINE void execute(CallFun_5_0 a) {
    (*a.fun)(a.src1, a.src2, a.src3, a.src4, a.src5);
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

  FOLLY_ALWAYS_INLINE void execute(Suspend a) {
    pc += std::ptrdiff_t(a.cont);
    (*a.fun)(const_cast<uint64_t*>(pc), frame);
  }

  FOLLY_ALWAYS_INLINE void execute(Ret) {}

  FOLLY_ALWAYS_INLINE void execute(LoadWord a) {
    *a.dst = *a.src;
  }

  FOLLY_ALWAYS_INLINE void execute(StoreWord a) {
    *a.dst = a.src;
  }
};

}

#define USE_SWITCH 1

void Subroutine::execute(const uint64_t *args) const {
  uint64_t frame[inputs + locals];
  std::copy(args, args + inputs, frame);
  assert(constants.size() <= locals);
  std::copy(constants.begin(), constants.end(), frame + inputs);
  Eval{literals.data(), code.data(), code.data(), frame}.
#if USE_SWITCH
    evalSwitch();
#else
    evalIndirect();
#endif
}

void Subroutine::restart(uint64_t *regs, uint64_t offset) const {
  Eval{literals.data(), code.data(), code.data() + offset, regs}.
#if USE_SWITCH
    evalSwitch();
#else
    evalIndirect();
#endif
}

bool Subroutine::operator==(const Subroutine& other) const {
  return this == &other ||
    (code == other.code
    && inputs == other.inputs
    && outputs == other.outputs
    && locals == other.locals
    && constants == other.constants
    && literals == other.literals);
}

size_t Subroutine::size() const {
  size_t litsz = 0;
  for (auto &lit : literals) {
    litsz += lit.size();
  }
  return
    litsz +
    sizeof(this) +
    code.size() * sizeof(uint64_t) +
    constants.size() * sizeof(uint64_t);
}

namespace {
template<typename T, typename U>
std::vector<T> copy_as(const std::vector<U>& xs) {
  std::vector<T> ys;
  ys.reserve(xs.size());
  for (const auto& x : xs) {
    ys.push_back(static_cast<T>(x));
  }
  return ys;
}
}

std::shared_ptr<Subroutine>
Subroutine::fromThrift(const thrift::internal::Subroutine &ser) {
  return std::make_shared<Subroutine>(Subroutine{
      copy_as<uint64_t>(ser.get_code()), static_cast<size_t>(ser.get_inputs()),
      static_cast<size_t>(ser.get_outputs()),
      static_cast<size_t>(ser.get_locals()),
      copy_as<uint64_t>(ser.get_constants()), ser.get_literals()});
}

thrift::internal::Subroutine Subroutine::toThrift(const Subroutine &sub) {
  thrift::internal::Subroutine ser = {};
  ser.code_ref().emplace(copy_as<int64_t>(sub.code));
  ser.inputs_ref().emplace(sub.inputs);
  ser.outputs_ref() = sub.outputs;
  ser.locals_ref() = sub.locals;
  ser.constants_ref().emplace(copy_as<int64_t>(sub.constants));
  ser.literals_ref().emplace(sub.literals);
  return ser;
}

}
}
}
