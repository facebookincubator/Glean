// Copyright (c) Facebook, Inc. and its affiliates.

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
