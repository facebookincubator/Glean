namespace N0 {

int f1(int);
int f1(const char *);
int f1(bool);

int f2();

namespace { int f3() {return 1;} int f3(int) {return 1;} }
inline namespace IN1 { int f4(); }

}

namespace N0 {

int f5();

}

namespace N1 {

using N0::f1;
using N0::f2;
using N0::f3;

}

namespace N1 {

using N0::f4;
using N0::f5;

}

namespace N2 {

using N1::f1;
using N1::f3;
using N1::f5;

int yes1 = f1(5);
int no1 = N1::f1(5);
int yes2 = f3();
int no2 = N1::f3();
int yes3 = f5();
int no3 = N1::f5();
int yes4 = f1("hello");
int no4 = N1::f1("hello");

}

namespace N2 {

int yes6 = f1(42);
int no6 = N1::f1(42);

namespace IN2 {

int yes9 = f3();
int no9 = N1::f3();
int yes10 = N2::f3();
int no10 = ::N1::f3();
int yes11 = N2::f5();
int no11 = ::N1::f5();

}

namespace IN3 {

using N1::f1;

int yes12 = f1(18);
int no12 = N1::f1(18);

}

}
