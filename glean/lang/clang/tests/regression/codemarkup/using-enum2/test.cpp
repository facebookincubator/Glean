namespace N1 { namespace O1 {}}

namespace N1 {
namespace N2 {
namespace N3 {
}
}
}

namespace N1 {
namespace N2 {
namespace N3 {
enum X { X1 = 0, X2 = 1, X3 = 2 };
}
}
}

namespace N1::O1 {
using namespace N2::N3;
void foo() { X1; }
}
