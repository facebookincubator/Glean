#define TEST(x) ((x)+1)

namespace facebook {

void f(int);

extern int global;

struct D;

struct D {
  int x;
};

class C;

class C : D {
 public:
  using T = int;

  C();
  C(T);
  ~C();

  T get();

 private:
  int a;
};

enum e { p, q };

enum class EE { r, s };

namespace glean {
  namespace worklist {
    struct Counter {
      struct Value {};
    };
  }
}

}
