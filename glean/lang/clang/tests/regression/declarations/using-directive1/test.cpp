/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
