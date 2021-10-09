// Copyright (c) Facebook, Inc. and its affiliates.

namespace N1 {
  namespace IN1 {
    namespace IIN1 {}
  }

  namespace IN2 {}
}

namespace T1 {

using namespace N1;
using namespace N1::IN1;
using namespace N1::IN1::IIN1;
using namespace IIN1;
using namespace IN2;

}

namespace N2 = N1;

namespace T2 {

using namespace N1;
using namespace N1::IN1;
using namespace N1::IN1::IIN1;
using namespace IIN1;
using namespace IN2;

}
