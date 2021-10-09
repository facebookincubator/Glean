extern int v1;
int v2;
static int v3;
inline int v4;
constexpr int v5 = 4;

struct T {
  static int m1;
  inline static int m2;
  constexpr static int m3 = 4;

  static void f() {
    m1;
    m2;
    m3;
  }
};

void g(int x1) {
  v1;
  v2;
  v3;
  v4;
  v5;
  T::m1;
  T::m2;
  T::m3;

  int x2;
  x1;
  x2;
}
