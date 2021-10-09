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

namespace IN2 { int f6(); }

}

namespace N1 {

using namespace N0;

}

namespace N2 {

using namespace N1;

int yes1 = f1(5);
int no1 = N1::f1(5);
int yes2 = f3();
int no2 = N1::f3();
int yes3 = f5();
int no3 = N1::f5();
int yes4 = f1("hello");
int no4 = N1::f1("hello");
int yes5 = IN2::f6();
int no5 = N1::IN2::f6();

}

namespace N2 {

int yes6 = f1(42);
int no6 = N1::f1(42);

namespace IN3 {

int yes9 = f3();
int no9 = N1::f3();
int yes10 = N2::f3();
int no10 = ::N1::f3();
int yes11 = N2::f5();
int no11 = ::N1::f5();

}

}
