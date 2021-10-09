namespace N1 {

int f1();

struct T1 {};

}

namespace N2 {

using namespace N1;

}

namespace N3 {

using N2::f1;
using N2::T1;

}

namespace N4 {

using namespace N3;

T1 x;
int y = f1();

}
