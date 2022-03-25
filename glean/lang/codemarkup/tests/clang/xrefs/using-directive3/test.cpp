/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N0 {
namespace N1 {
namespace N2 {}
}
}

namespace X1 {
using namespace N0;
using namespace N1;
using namespace N2;
}

namespace X2 {

using namespace N0;

}

namespace X3 {

using namespace X2::N1;

}
