// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once
#include <cstdint>

constexpr uint64_t uniform64(uint64_t x) {
  return x * 11400714819323198485lu;
}

constexpr uint64_t uniform28(uint64_t x) {
  return uniform64(x) & 0x0FFFFFFF;
}

constexpr uint64_t uniform14(uint64_t x) {
  return uniform64(x) & 0x3FFF;
}

constexpr uint64_t uniform7(uint64_t x) {
  return uniform64(x) & 0x7F;
}
