// Copyright (c) Facebook, Inc. and its affiliates.

template<typename T>
struct U1 {
  unsigned x : sizeof(T);
};

template<unsigned N>
struct U2 {
  unsigned x : N;
};

template<typename T>
void f1(T) {
  struct Local {
    unsigned x : sizeof(T);
  };
}

template<unsigned N>
void f2() {
  struct Local {
    unsigned x : N;
  };
}
