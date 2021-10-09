namespace N1 {

struct T {};

}

using namespace N1;

namespace N1 {

void foo() {
  ::T x;
}

}
