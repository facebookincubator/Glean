// Copyright (c) Facebook, Inc. and its affiliates.

namespace N1 {

enum E { A, B, C };

}

namespace N21{

using namespace N1;
using N1::E;

void foo() {
  A;  // should go with using N1::A, not E
}

}

namespace N22 {

using N1::E;
using namespace N1;

void foo() {
  A;  // should go with using N1::A, not E
}

}

namespace N31 {

using N1::A;
using N1::E;

void foo() {
  A;
}

}

namespace N32 {

using N1::E;
using N1::A;

void foo() {
  A;
}

}

namespace N41 {

using namespace N21;
using N1::E;

void bar() {
  A;
}

}

namespace N42 {

using N1::E;
using namespace N22;

void bar() {
  A;
}

}
