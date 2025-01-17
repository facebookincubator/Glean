// @generated

  FOLLY_ALWAYS_INLINE const uint64_t * FOLLY_NULLABLE evalSwitch() {
    while (true) {
      switch (static_cast<Op>(*pc++)) {
        case Op::InputNat:
          eval_InputNat();
          break;
  
        case Op::InputBytes:
          eval_InputBytes();
          break;
  
        case Op::InputSkipUntrustedString:
          eval_InputSkipUntrustedString();
          break;
  
        case Op::InputShiftLit:
          eval_InputShiftLit();
          break;
  
        case Op::InputShiftBytes:
          eval_InputShiftBytes();
          break;
  
        case Op::InputSkipNat:
          eval_InputSkipNat();
          break;
  
        case Op::InputSkipTrustedString:
          eval_InputSkipTrustedString();
          break;
  
        case Op::ResetOutput:
          eval_ResetOutput();
          break;
  
        case Op::OutputNat:
          eval_OutputNat();
          break;
  
        case Op::OutputNatImm:
          eval_OutputNatImm();
          break;
  
        case Op::OutputByte:
          eval_OutputByte();
          break;
  
        case Op::OutputByteImm:
          eval_OutputByteImm();
          break;
  
        case Op::OutputBytes:
          eval_OutputBytes();
          break;
  
        case Op::OutputStringToLower:
          eval_OutputStringToLower();
          break;
  
        case Op::OutputRelToAbsByteSpans:
          eval_OutputRelToAbsByteSpans();
          break;
  
        case Op::OutputUnpackByteSpans:
          eval_OutputUnpackByteSpans();
          break;
  
        case Op::OutputStringReverse:
          eval_OutputStringReverse();
          break;
  
        case Op::GetOutput:
          eval_GetOutput();
          break;
  
        case Op::GetOutputSize:
          eval_GetOutputSize();
          break;
  
        case Op::LoadConst:
          eval_LoadConst();
          break;
  
        case Op::LoadLiteral:
          eval_LoadLiteral();
          break;
  
        case Op::Move:
          eval_Move();
          break;
  
        case Op::SubConst:
          eval_SubConst();
          break;
  
        case Op::Sub:
          eval_Sub();
          break;
  
        case Op::AddConst:
          eval_AddConst();
          break;
  
        case Op::Add:
          eval_Add();
          break;
  
        case Op::PtrDiff:
          eval_PtrDiff();
          break;
  
        case Op::LoadLabel:
          eval_LoadLabel();
          break;
  
        case Op::Jump:
          eval_Jump();
          break;
  
        case Op::JumpReg:
          eval_JumpReg();
          break;
  
        case Op::JumpIf0:
          eval_JumpIf0();
          break;
  
        case Op::JumpIfNot0:
          eval_JumpIfNot0();
          break;
  
        case Op::JumpIfEq:
          eval_JumpIfEq();
          break;
  
        case Op::JumpIfNe:
          eval_JumpIfNe();
          break;
  
        case Op::JumpIfGt:
          eval_JumpIfGt();
          break;
  
        case Op::JumpIfGe:
          eval_JumpIfGe();
          break;
  
        case Op::JumpIfLt:
          eval_JumpIfLt();
          break;
  
        case Op::JumpIfLe:
          eval_JumpIfLe();
          break;
  
        case Op::DecrAndJumpIfNot0:
          eval_DecrAndJumpIfNot0();
          break;
  
        case Op::DecrAndJumpIf0:
          eval_DecrAndJumpIf0();
          break;
  
        case Op::CallFun_0_1:
          eval_CallFun_0_1();
          break;
  
        case Op::CallFun_0_2:
          eval_CallFun_0_2();
          break;
  
        case Op::CallFun_1_1:
          eval_CallFun_1_1();
          break;
  
        case Op::CallFun_1_0:
          eval_CallFun_1_0();
          break;
  
        case Op::CallFun_2_1:
          eval_CallFun_2_1();
          break;
  
        case Op::CallFun_2_0:
          eval_CallFun_2_0();
          break;
  
        case Op::CallFun_3_0:
          eval_CallFun_3_0();
          break;
  
        case Op::CallFun_4_0:
          eval_CallFun_4_0();
          break;
  
        case Op::CallFun_3_1:
          eval_CallFun_3_1();
          break;
  
        case Op::CallFun_5_0:
          eval_CallFun_5_0();
          break;
  
        case Op::CallFun_5_1:
          eval_CallFun_5_1();
          break;
  
        case Op::CallFun_2_2:
          eval_CallFun_2_2();
          break;
  
        case Op::CallFun_2_5:
          eval_CallFun_2_5();
          break;
  
        case Op::Select:
          eval_Select();
          break;
  
        case Op::Raise:
          eval_Raise();
          break;
  
        case Op::Trace:
          eval_Trace();
          break;
  
        case Op::TraceReg:
          eval_TraceReg();
          break;
  
        case Op::Suspend:
          return eval_Suspend();
  
        case Op::Ret:
          return eval_Ret();
  
        case Op::Unused59:
        case Op::Unused60:
        case Op::Unused61:
        case Op::Unused62:
        case Op::Unused63:
        case Op::Unused64:
        case Op::Unused65:
        case Op::Unused66:
        case Op::Unused67:
        case Op::Unused68:
        case Op::Unused69:
        case Op::Unused70:
        case Op::Unused71:
        case Op::Unused72:
        case Op::Unused73:
        case Op::Unused74:
        case Op::Unused75:
        case Op::Unused76:
        case Op::Unused77:
        case Op::Unused78:
        case Op::Unused79:
        case Op::Unused80:
        case Op::Unused81:
        case Op::Unused82:
        case Op::Unused83:
        case Op::Unused84:
        case Op::Unused85:
        case Op::Unused86:
        case Op::Unused87:
        case Op::Unused88:
        case Op::Unused89:
        case Op::Unused90:
        case Op::Unused91:
        case Op::Unused92:
        case Op::Unused93:
        case Op::Unused94:
        case Op::Unused95:
        case Op::Unused96:
        case Op::Unused97:
        case Op::Unused98:
        case Op::Unused99:
        case Op::Unused100:
        case Op::Unused101:
        case Op::Unused102:
        case Op::Unused103:
        case Op::Unused104:
        case Op::Unused105:
        case Op::Unused106:
        case Op::Unused107:
        case Op::Unused108:
        case Op::Unused109:
        case Op::Unused110:
        case Op::Unused111:
        case Op::Unused112:
        case Op::Unused113:
        case Op::Unused114:
        case Op::Unused115:
        case Op::Unused116:
        case Op::Unused117:
        case Op::Unused118:
        case Op::Unused119:
        case Op::Unused120:
        case Op::Unused121:
        case Op::Unused122:
        case Op::Unused123:
        case Op::Unused124:
        case Op::Unused125:
        case Op::Unused126:
        case Op::Unused127:
        case Op::Unused128:
        case Op::Unused129:
        case Op::Unused130:
        case Op::Unused131:
        case Op::Unused132:
        case Op::Unused133:
        case Op::Unused134:
        case Op::Unused135:
        case Op::Unused136:
        case Op::Unused137:
        case Op::Unused138:
        case Op::Unused139:
        case Op::Unused140:
        case Op::Unused141:
        case Op::Unused142:
        case Op::Unused143:
        case Op::Unused144:
        case Op::Unused145:
        case Op::Unused146:
        case Op::Unused147:
        case Op::Unused148:
        case Op::Unused149:
        case Op::Unused150:
        case Op::Unused151:
        case Op::Unused152:
        case Op::Unused153:
        case Op::Unused154:
        case Op::Unused155:
        case Op::Unused156:
        case Op::Unused157:
        case Op::Unused158:
        case Op::Unused159:
        case Op::Unused160:
        case Op::Unused161:
        case Op::Unused162:
        case Op::Unused163:
        case Op::Unused164:
        case Op::Unused165:
        case Op::Unused166:
        case Op::Unused167:
        case Op::Unused168:
        case Op::Unused169:
        case Op::Unused170:
        case Op::Unused171:
        case Op::Unused172:
        case Op::Unused173:
        case Op::Unused174:
        case Op::Unused175:
        case Op::Unused176:
        case Op::Unused177:
        case Op::Unused178:
        case Op::Unused179:
        case Op::Unused180:
        case Op::Unused181:
        case Op::Unused182:
        case Op::Unused183:
        case Op::Unused184:
        case Op::Unused185:
        case Op::Unused186:
        case Op::Unused187:
        case Op::Unused188:
        case Op::Unused189:
        case Op::Unused190:
        case Op::Unused191:
        case Op::Unused192:
        case Op::Unused193:
        case Op::Unused194:
        case Op::Unused195:
        case Op::Unused196:
        case Op::Unused197:
        case Op::Unused198:
        case Op::Unused199:
        case Op::Unused200:
        case Op::Unused201:
        case Op::Unused202:
        case Op::Unused203:
        case Op::Unused204:
        case Op::Unused205:
        case Op::Unused206:
        case Op::Unused207:
        case Op::Unused208:
        case Op::Unused209:
        case Op::Unused210:
        case Op::Unused211:
        case Op::Unused212:
        case Op::Unused213:
        case Op::Unused214:
        case Op::Unused215:
        case Op::Unused216:
        case Op::Unused217:
        case Op::Unused218:
        case Op::Unused219:
        case Op::Unused220:
        case Op::Unused221:
        case Op::Unused222:
        case Op::Unused223:
        case Op::Unused224:
        case Op::Unused225:
        case Op::Unused226:
        case Op::Unused227:
        case Op::Unused228:
        case Op::Unused229:
        case Op::Unused230:
        case Op::Unused231:
        case Op::Unused232:
        case Op::Unused233:
        case Op::Unused234:
        case Op::Unused235:
        case Op::Unused236:
        case Op::Unused237:
        case Op::Unused238:
        case Op::Unused239:
        case Op::Unused240:
        case Op::Unused241:
        case Op::Unused242:
        case Op::Unused243:
        case Op::Unused244:
        case Op::Unused245:
        case Op::Unused246:
        case Op::Unused247:
        case Op::Unused248:
        case Op::Unused249:
        case Op::Unused250:
        case Op::Unused251:
        case Op::Unused252:
        case Op::Unused253:
        case Op::Unused254:
        case Op::Unused255:
          rts::error("invalid opcode");
      }
    }
  }

  FOLLY_ALWAYS_INLINE const uint64_t * FOLLY_NULLABLE evalIndirect() {
    static const void * const labels[] = {
      &&label_InputNat,
      &&label_InputBytes,
      &&label_InputSkipUntrustedString,
      &&label_InputShiftLit,
      &&label_InputShiftBytes,
      &&label_InputSkipNat,
      &&label_InputSkipTrustedString,
      &&label_ResetOutput,
      &&label_OutputNat,
      &&label_OutputNatImm,
      &&label_OutputByte,
      &&label_OutputByteImm,
      &&label_OutputBytes,
      &&label_OutputStringToLower,
      &&label_OutputRelToAbsByteSpans,
      &&label_OutputUnpackByteSpans,
      &&label_OutputStringReverse,
      &&label_GetOutput,
      &&label_GetOutputSize,
      &&label_LoadConst,
      &&label_LoadLiteral,
      &&label_Move,
      &&label_SubConst,
      &&label_Sub,
      &&label_AddConst,
      &&label_Add,
      &&label_PtrDiff,
      &&label_LoadLabel,
      &&label_Jump,
      &&label_JumpReg,
      &&label_JumpIf0,
      &&label_JumpIfNot0,
      &&label_JumpIfEq,
      &&label_JumpIfNe,
      &&label_JumpIfGt,
      &&label_JumpIfGe,
      &&label_JumpIfLt,
      &&label_JumpIfLe,
      &&label_DecrAndJumpIfNot0,
      &&label_DecrAndJumpIf0,
      &&label_CallFun_0_1,
      &&label_CallFun_0_2,
      &&label_CallFun_1_1,
      &&label_CallFun_1_0,
      &&label_CallFun_2_1,
      &&label_CallFun_2_0,
      &&label_CallFun_3_0,
      &&label_CallFun_4_0,
      &&label_CallFun_3_1,
      &&label_CallFun_5_0,
      &&label_CallFun_5_1,
      &&label_CallFun_2_2,
      &&label_CallFun_2_5,
      &&label_Select,
      &&label_Raise,
      &&label_Trace,
      &&label_TraceReg,
      &&label_Suspend,
      &&label_Ret,
    };
  
    goto *labels[*pc++];
  
  label_InputNat:
          eval_InputNat();
    goto *labels[*pc++];
  
  label_InputBytes:
          eval_InputBytes();
    goto *labels[*pc++];
  
  label_InputSkipUntrustedString:
          eval_InputSkipUntrustedString();
    goto *labels[*pc++];
  
  label_InputShiftLit:
          eval_InputShiftLit();
    goto *labels[*pc++];
  
  label_InputShiftBytes:
          eval_InputShiftBytes();
    goto *labels[*pc++];
  
  label_InputSkipNat:
          eval_InputSkipNat();
    goto *labels[*pc++];
  
  label_InputSkipTrustedString:
          eval_InputSkipTrustedString();
    goto *labels[*pc++];
  
  label_ResetOutput:
          eval_ResetOutput();
    goto *labels[*pc++];
  
  label_OutputNat:
          eval_OutputNat();
    goto *labels[*pc++];
  
  label_OutputNatImm:
          eval_OutputNatImm();
    goto *labels[*pc++];
  
  label_OutputByte:
          eval_OutputByte();
    goto *labels[*pc++];
  
  label_OutputByteImm:
          eval_OutputByteImm();
    goto *labels[*pc++];
  
  label_OutputBytes:
          eval_OutputBytes();
    goto *labels[*pc++];
  
  label_OutputStringToLower:
          eval_OutputStringToLower();
    goto *labels[*pc++];
  
  label_OutputRelToAbsByteSpans:
          eval_OutputRelToAbsByteSpans();
    goto *labels[*pc++];
  
  label_OutputUnpackByteSpans:
          eval_OutputUnpackByteSpans();
    goto *labels[*pc++];
  
  label_OutputStringReverse:
          eval_OutputStringReverse();
    goto *labels[*pc++];
  
  label_GetOutput:
          eval_GetOutput();
    goto *labels[*pc++];
  
  label_GetOutputSize:
          eval_GetOutputSize();
    goto *labels[*pc++];
  
  label_LoadConst:
          eval_LoadConst();
    goto *labels[*pc++];
  
  label_LoadLiteral:
          eval_LoadLiteral();
    goto *labels[*pc++];
  
  label_Move:
          eval_Move();
    goto *labels[*pc++];
  
  label_SubConst:
          eval_SubConst();
    goto *labels[*pc++];
  
  label_Sub:
          eval_Sub();
    goto *labels[*pc++];
  
  label_AddConst:
          eval_AddConst();
    goto *labels[*pc++];
  
  label_Add:
          eval_Add();
    goto *labels[*pc++];
  
  label_PtrDiff:
          eval_PtrDiff();
    goto *labels[*pc++];
  
  label_LoadLabel:
          eval_LoadLabel();
    goto *labels[*pc++];
  
  label_Jump:
          eval_Jump();
    goto *labels[*pc++];
  
  label_JumpReg:
          eval_JumpReg();
    goto *labels[*pc++];
  
  label_JumpIf0:
          eval_JumpIf0();
    goto *labels[*pc++];
  
  label_JumpIfNot0:
          eval_JumpIfNot0();
    goto *labels[*pc++];
  
  label_JumpIfEq:
          eval_JumpIfEq();
    goto *labels[*pc++];
  
  label_JumpIfNe:
          eval_JumpIfNe();
    goto *labels[*pc++];
  
  label_JumpIfGt:
          eval_JumpIfGt();
    goto *labels[*pc++];
  
  label_JumpIfGe:
          eval_JumpIfGe();
    goto *labels[*pc++];
  
  label_JumpIfLt:
          eval_JumpIfLt();
    goto *labels[*pc++];
  
  label_JumpIfLe:
          eval_JumpIfLe();
    goto *labels[*pc++];
  
  label_DecrAndJumpIfNot0:
          eval_DecrAndJumpIfNot0();
    goto *labels[*pc++];
  
  label_DecrAndJumpIf0:
          eval_DecrAndJumpIf0();
    goto *labels[*pc++];
  
  label_CallFun_0_1:
          eval_CallFun_0_1();
    goto *labels[*pc++];
  
  label_CallFun_0_2:
          eval_CallFun_0_2();
    goto *labels[*pc++];
  
  label_CallFun_1_1:
          eval_CallFun_1_1();
    goto *labels[*pc++];
  
  label_CallFun_1_0:
          eval_CallFun_1_0();
    goto *labels[*pc++];
  
  label_CallFun_2_1:
          eval_CallFun_2_1();
    goto *labels[*pc++];
  
  label_CallFun_2_0:
          eval_CallFun_2_0();
    goto *labels[*pc++];
  
  label_CallFun_3_0:
          eval_CallFun_3_0();
    goto *labels[*pc++];
  
  label_CallFun_4_0:
          eval_CallFun_4_0();
    goto *labels[*pc++];
  
  label_CallFun_3_1:
          eval_CallFun_3_1();
    goto *labels[*pc++];
  
  label_CallFun_5_0:
          eval_CallFun_5_0();
    goto *labels[*pc++];
  
  label_CallFun_5_1:
          eval_CallFun_5_1();
    goto *labels[*pc++];
  
  label_CallFun_2_2:
          eval_CallFun_2_2();
    goto *labels[*pc++];
  
  label_CallFun_2_5:
          eval_CallFun_2_5();
    goto *labels[*pc++];
  
  label_Select:
          eval_Select();
    goto *labels[*pc++];
  
  label_Raise:
          eval_Raise();
    goto *labels[*pc++];
  
  label_Trace:
          eval_Trace();
    goto *labels[*pc++];
  
  label_TraceReg:
          eval_TraceReg();
    goto *labels[*pc++];
  
  label_Suspend:
          return eval_Suspend();
  
  label_Ret:
          return eval_Ret();
  }

  struct InputNat {
    Reg<const unsigned char *> begin;
    const unsigned char * end;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_InputNat() {
    InputNat args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "InputNat" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << args.dst;
    return execute(args);
  }

  struct InputBytes {
    Reg<const unsigned char *> begin;
    const unsigned char * end;
    uint64_t size;
  };
  
  FOLLY_ALWAYS_INLINE void eval_InputBytes() {
    InputBytes args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.size = Reg<uint64_t>(&frame[*pc++]).get();
    DVLOG(5) << "InputBytes" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << args.size;
    return execute(args);
  }

  struct InputSkipUntrustedString {
    Reg<const unsigned char *> begin;
    const unsigned char * end;
  };
  
  FOLLY_ALWAYS_INLINE void eval_InputSkipUntrustedString() {
    InputSkipUntrustedString args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    DVLOG(5) << "InputSkipUntrustedString" << "  " << "<<ptr>>" << "  " << "<<ptr>>";
    return execute(args);
  }

  struct InputShiftLit {
    Reg<const unsigned char *> begin;
    const unsigned char * end;
    const std::string * lit;
    Reg<uint64_t> match;
  };
  
  FOLLY_ALWAYS_INLINE void eval_InputShiftLit() {
    InputShiftLit args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.lit = &literals[*pc++];
    args.match = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "InputShiftLit" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << args.lit << "  " << args.match;
    return execute(args);
  }

  struct InputShiftBytes {
    Reg<const unsigned char *> begin;
    const unsigned char * end;
    const unsigned char * ptr;
    const unsigned char * ptrend;
    Reg<uint64_t> match;
  };
  
  FOLLY_ALWAYS_INLINE void eval_InputShiftBytes() {
    InputShiftBytes args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.ptr = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.ptrend = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.match = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "InputShiftBytes" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << args.match;
    return execute(args);
  }

  struct InputSkipNat {
    Reg<const unsigned char *> begin;
    const unsigned char * end;
  };
  
  FOLLY_ALWAYS_INLINE void eval_InputSkipNat() {
    InputSkipNat args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    DVLOG(5) << "InputSkipNat" << "  " << "<<ptr>>" << "  " << "<<ptr>>";
    return execute(args);
  }

  struct InputSkipTrustedString {
    Reg<const unsigned char *> begin;
    const unsigned char * end;
  };
  
  FOLLY_ALWAYS_INLINE void eval_InputSkipTrustedString() {
    InputSkipTrustedString args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    DVLOG(5) << "InputSkipTrustedString" << "  " << "<<ptr>>" << "  " << "<<ptr>>";
    return execute(args);
  }

  struct ResetOutput {
    binary::Output * output;
  };
  
  FOLLY_ALWAYS_INLINE void eval_ResetOutput() {
    ResetOutput args;
    args.output = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "ResetOutput" << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputNat {
    uint64_t src;
    binary::Output * output;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputNat() {
    OutputNat args;
    args.src = Reg<uint64_t>(&frame[*pc++]).get();
    args.output = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputNat" << "  " << args.src << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputNatImm {
    uint64_t src;
    binary::Output * output;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputNatImm() {
    OutputNatImm args;
    args.src = *pc++;
    args.output = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputNatImm" << "  " << args.src << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputByte {
    uint64_t src;
    binary::Output * output;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputByte() {
    OutputByte args;
    args.src = Reg<uint64_t>(&frame[*pc++]).get();
    args.output = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputByte" << "  " << args.src << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputByteImm {
    uint64_t src;
    binary::Output * output;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputByteImm() {
    OutputByteImm args;
    args.src = *pc++;
    args.output = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputByteImm" << "  " << args.src << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputBytes {
    const unsigned char * ptr;
    const unsigned char * end;
    binary::Output * output;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputBytes() {
    OutputBytes args;
    args.ptr = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.output = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputBytes" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputStringToLower {
    const unsigned char * begin;
    const unsigned char * end;
    binary::Output * dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputStringToLower() {
    OutputStringToLower args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.dst = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputStringToLower" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputRelToAbsByteSpans {
    const unsigned char * begin;
    const unsigned char * end;
    binary::Output * dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputRelToAbsByteSpans() {
    OutputRelToAbsByteSpans args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.dst = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputRelToAbsByteSpans" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputUnpackByteSpans {
    const unsigned char * begin;
    const unsigned char * end;
    binary::Output * dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputUnpackByteSpans() {
    OutputUnpackByteSpans args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.dst = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputUnpackByteSpans" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct OutputStringReverse {
    const unsigned char * begin;
    const unsigned char * end;
    binary::Output * dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_OutputStringReverse() {
    OutputStringReverse args;
    args.begin = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.end = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.dst = Reg<binary::Output *>(&frame[*pc++]).get();
    DVLOG(5) << "OutputStringReverse" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << "<<binary::Output>>";
    return execute(args);
  }

  struct GetOutput {
    binary::Output * output;
    Reg<const unsigned char *> ptr;
    Reg<const unsigned char *> end;
  };
  
  FOLLY_ALWAYS_INLINE void eval_GetOutput() {
    GetOutput args;
    args.output = Reg<binary::Output *>(&frame[*pc++]).get();
    args.ptr = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]);
    DVLOG(5) << "GetOutput" << "  " << "<<binary::Output>>" << "  " << "<<ptr>>" << "  " << "<<ptr>>";
    return execute(args);
  }

  struct GetOutputSize {
    binary::Output * output;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_GetOutputSize() {
    GetOutputSize args;
    args.output = Reg<binary::Output *>(&frame[*pc++]).get();
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "GetOutputSize" << "  " << "<<binary::Output>>" << "  " << args.dst;
    return execute(args);
  }

  struct LoadConst {
    uint64_t imm;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_LoadConst() {
    LoadConst args;
    args.imm = *pc++;
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "LoadConst" << "  " << args.imm << "  " << args.dst;
    return execute(args);
  }

  struct LoadLiteral {
    const std::string * lit;
    Reg<const unsigned char *> ptr;
    Reg<const unsigned char *> end;
  };
  
  FOLLY_ALWAYS_INLINE void eval_LoadLiteral() {
    LoadLiteral args;
    args.lit = &literals[*pc++];
    args.ptr = Reg<const unsigned char *>(&frame[*pc++]);
    args.end = Reg<const unsigned char *>(&frame[*pc++]);
    DVLOG(5) << "LoadLiteral" << "  " << args.lit << "  " << "<<ptr>>" << "  " << "<<ptr>>";
    return execute(args);
  }

  struct Move {
    uint64_t src;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_Move() {
    Move args;
    args.src = Reg<uint64_t>(&frame[*pc++]).get();
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "Move" << "  " << args.src << "  " << args.dst;
    return execute(args);
  }

  struct SubConst {
    uint64_t imm;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_SubConst() {
    SubConst args;
    args.imm = *pc++;
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "SubConst" << "  " << args.imm << "  " << args.dst;
    return execute(args);
  }

  struct Sub {
    uint64_t src;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_Sub() {
    Sub args;
    args.src = Reg<uint64_t>(&frame[*pc++]).get();
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "Sub" << "  " << args.src << "  " << args.dst;
    return execute(args);
  }

  struct AddConst {
    uint64_t imm;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_AddConst() {
    AddConst args;
    args.imm = *pc++;
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "AddConst" << "  " << args.imm << "  " << args.dst;
    return execute(args);
  }

  struct Add {
    uint64_t src;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_Add() {
    Add args;
    args.src = Reg<uint64_t>(&frame[*pc++]).get();
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "Add" << "  " << args.src << "  " << args.dst;
    return execute(args);
  }

  struct PtrDiff {
    const unsigned char * src1;
    const unsigned char * src2;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_PtrDiff() {
    PtrDiff args;
    args.src1 = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.src2 = Reg<const unsigned char *>(&frame[*pc++]).get();
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "PtrDiff" << "  " << "<<ptr>>" << "  " << "<<ptr>>" << "  " << args.dst;
    return execute(args);
  }

  struct LoadLabel {
    uint64_t lbl;
    Reg<uint64_t> dst;
  };
  
  FOLLY_ALWAYS_INLINE void eval_LoadLabel() {
    LoadLabel args;
    args.lbl = *pc++;
    args.dst = Reg<uint64_t>(&frame[*pc++]);
    DVLOG(5) << "LoadLabel" << "  " << args.lbl << "  " << args.dst;
    return execute(args);
  }

  struct Jump {
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_Jump() {
    Jump args;
    args.tgt = *pc++;
    DVLOG(5) << "Jump" << "  " << args.tgt;
    return execute(args);
  }

  struct JumpReg {
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpReg() {
    JumpReg args;
    args.tgt = Reg<uint64_t>(&frame[*pc++]).get();
    DVLOG(5) << "JumpReg" << "  " << args.tgt;
    return execute(args);
  }

  struct JumpIf0 {
    uint64_t reg;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpIf0() {
    JumpIf0 args;
    args.reg = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgt = *pc++;
    DVLOG(5) << "JumpIf0" << "  " << args.reg << "  " << args.tgt;
    return execute(args);
  }

  struct JumpIfNot0 {
    uint64_t reg;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpIfNot0() {
    JumpIfNot0 args;
    args.reg = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgt = *pc++;
    DVLOG(5) << "JumpIfNot0" << "  " << args.reg << "  " << args.tgt;
    return execute(args);
  }

  struct JumpIfEq {
    uint64_t reg1;
    uint64_t reg2;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpIfEq() {
    JumpIfEq args;
    args.reg1 = Reg<uint64_t>(&frame[*pc++]).get();
    args.reg2 = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgt = *pc++;
    DVLOG(5) << "JumpIfEq" << "  " << args.reg1 << "  " << args.reg2 << "  " << args.tgt;
    return execute(args);
  }

  struct JumpIfNe {
    uint64_t reg1;
    uint64_t reg2;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpIfNe() {
    JumpIfNe args;
    args.reg1 = Reg<uint64_t>(&frame[*pc++]).get();
    args.reg2 = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgt = *pc++;
    DVLOG(5) << "JumpIfNe" << "  " << args.reg1 << "  " << args.reg2 << "  " << args.tgt;
    return execute(args);
  }

  struct JumpIfGt {
    uint64_t reg1;
    uint64_t reg2;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpIfGt() {
    JumpIfGt args;
    args.reg1 = Reg<uint64_t>(&frame[*pc++]).get();
    args.reg2 = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgt = *pc++;
    DVLOG(5) << "JumpIfGt" << "  " << args.reg1 << "  " << args.reg2 << "  " << args.tgt;
    return execute(args);
  }

  struct JumpIfGe {
    uint64_t reg1;
    uint64_t reg2;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpIfGe() {
    JumpIfGe args;
    args.reg1 = Reg<uint64_t>(&frame[*pc++]).get();
    args.reg2 = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgt = *pc++;
    DVLOG(5) << "JumpIfGe" << "  " << args.reg1 << "  " << args.reg2 << "  " << args.tgt;
    return execute(args);
  }

  struct JumpIfLt {
    uint64_t reg1;
    uint64_t reg2;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpIfLt() {
    JumpIfLt args;
    args.reg1 = Reg<uint64_t>(&frame[*pc++]).get();
    args.reg2 = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgt = *pc++;
    DVLOG(5) << "JumpIfLt" << "  " << args.reg1 << "  " << args.reg2 << "  " << args.tgt;
    return execute(args);
  }

  struct JumpIfLe {
    uint64_t reg1;
    uint64_t reg2;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_JumpIfLe() {
    JumpIfLe args;
    args.reg1 = Reg<uint64_t>(&frame[*pc++]).get();
    args.reg2 = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgt = *pc++;
    DVLOG(5) << "JumpIfLe" << "  " << args.reg1 << "  " << args.reg2 << "  " << args.tgt;
    return execute(args);
  }

  struct DecrAndJumpIfNot0 {
    Reg<uint64_t> reg;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_DecrAndJumpIfNot0() {
    DecrAndJumpIfNot0 args;
    args.reg = Reg<uint64_t>(&frame[*pc++]);
    args.tgt = *pc++;
    DVLOG(5) << "DecrAndJumpIfNot0" << "  " << args.reg << "  " << args.tgt;
    return execute(args);
  }

  struct DecrAndJumpIf0 {
    Reg<uint64_t> reg;
    uint64_t tgt;
  };
  
  FOLLY_ALWAYS_INLINE void eval_DecrAndJumpIf0() {
    DecrAndJumpIf0 args;
    args.reg = Reg<uint64_t>(&frame[*pc++]);
    args.tgt = *pc++;
    DVLOG(5) << "DecrAndJumpIf0" << "  " << args.reg << "  " << args.tgt;
    return execute(args);
  }

  struct CallFun_0_1 {
    SysFun fun;
    static constexpr uint64_t args_arity = 1;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_0_1() {
    CallFun_0_1 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_0_1" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_0_2 {
    SysFun fun;
    static constexpr uint64_t args_arity = 2;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_0_2() {
    CallFun_0_2 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_0_2" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_1_1 {
    SysFun fun;
    static constexpr uint64_t args_arity = 2;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_1_1() {
    CallFun_1_1 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_1_1" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_1_0 {
    SysFun fun;
    static constexpr uint64_t args_arity = 1;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_1_0() {
    CallFun_1_0 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_1_0" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_2_1 {
    SysFun fun;
    static constexpr uint64_t args_arity = 3;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_2_1() {
    CallFun_2_1 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_2_1" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_2_0 {
    SysFun fun;
    static constexpr uint64_t args_arity = 2;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_2_0() {
    CallFun_2_0 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_2_0" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_3_0 {
    SysFun fun;
    static constexpr uint64_t args_arity = 3;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_3_0() {
    CallFun_3_0 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_3_0" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_4_0 {
    SysFun fun;
    static constexpr uint64_t args_arity = 4;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_4_0() {
    CallFun_4_0 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_4_0" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_3_1 {
    SysFun fun;
    static constexpr uint64_t args_arity = 4;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_3_1() {
    CallFun_3_1 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_3_1" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_5_0 {
    SysFun fun;
    static constexpr uint64_t args_arity = 5;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_5_0() {
    CallFun_5_0 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_5_0" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_5_1 {
    SysFun fun;
    static constexpr uint64_t args_arity = 6;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_5_1() {
    CallFun_5_1 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_5_1" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_2_2 {
    SysFun fun;
    static constexpr uint64_t args_arity = 4;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_2_2() {
    CallFun_2_2 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_2_2" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct CallFun_2_5 {
    SysFun fun;
    static constexpr uint64_t args_arity = 7;
    const uint64_t *args;
  };
  
  FOLLY_ALWAYS_INLINE void eval_CallFun_2_5() {
    CallFun_2_5 args;
    args.fun = Reg<SysFun>(&frame[*pc++]).get();
    args.args = pc;
    pc += args.args_arity;
    DVLOG(5) << "CallFun_2_5" << "  " << "<<funptr>>" << "  " << "<<reg arguments>>";
    return execute(args);
  }

  struct Select {
    uint64_t sel;
    uint64_t tgts_size;
    const uint64_t *tgts;
  };
  
  FOLLY_ALWAYS_INLINE void eval_Select() {
    Select args;
    args.sel = Reg<uint64_t>(&frame[*pc++]).get();
    args.tgts_size = *pc++;
    args.tgts = pc;
    pc += args.tgts_size;
    DVLOG(5) << "Select" << "  " << args.sel << "  " << "<<offsets>>";
    return execute(args);
  }

  struct Raise {
    const std::string * msg;
  };
  
  FOLLY_ALWAYS_INLINE void eval_Raise() {
    Raise args;
    args.msg = &literals[*pc++];
    DVLOG(5) << "Raise" << "  " << args.msg;
    return execute(args);
  }

  struct Trace {
    const std::string * msg;
  };
  
  FOLLY_ALWAYS_INLINE void eval_Trace() {
    Trace args;
    args.msg = &literals[*pc++];
    DVLOG(5) << "Trace" << "  " << args.msg;
    return execute(args);
  }

  struct TraceReg {
    const std::string * msg;
    uint64_t reg;
  };
  
  FOLLY_ALWAYS_INLINE void eval_TraceReg() {
    TraceReg args;
    args.msg = &literals[*pc++];
    args.reg = Reg<uint64_t>(&frame[*pc++]).get();
    DVLOG(5) << "TraceReg" << "  " << args.msg << "  " << args.reg;
    return execute(args);
  }

  struct Suspend {
    uint64_t unused;
    uint64_t cont;
  };
  
  FOLLY_ALWAYS_INLINE const uint64_t * FOLLY_NULLABLE  eval_Suspend() {
    Suspend args;
    args.unused = Reg<uint64_t>(&frame[*pc++]).get();
    args.cont = *pc++;
    DVLOG(5) << "Suspend" << "  " << args.unused << "  " << args.cont;
    return execute(args);
  }

  struct Ret {
  };
  
  FOLLY_ALWAYS_INLINE const uint64_t * FOLLY_NULLABLE  eval_Ret() {
    Ret args;
    DVLOG(5) << "Ret";
    return execute(args);
  }
