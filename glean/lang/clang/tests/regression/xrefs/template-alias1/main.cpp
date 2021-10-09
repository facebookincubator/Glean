// Copyright (c) Facebook, Inc. and its affiliates.

template<typename T>
struct S {};

template<typename T> using F1 = T;
template<typename T> using F2 = int;
template<typename T> using F3 = S<T>;

F1<int> x;
F2<int> y;
F3<int> z;
