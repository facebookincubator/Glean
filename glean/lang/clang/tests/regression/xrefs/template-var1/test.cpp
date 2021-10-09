// Copyright (c) Facebook, Inc. and its affiliates.

template<typename T> constexpr int foo = 1;
template<typename T> constexpr int foo<T*> = 2;
template<> constexpr int foo<bool> = 3;
template constexpr int foo<void>;

void h() {
  foo<int>;
  foo<int*>;
  foo<bool>;
  foo<void>;
}
