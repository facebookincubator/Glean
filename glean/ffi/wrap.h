// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <cstring>
#include <stdexcept>

namespace facebook {
namespace glean {
namespace ffi {

extern const char *outOfMemory;
extern const char *unknownError;

template<typename F>
const char *wrap(F&& f) noexcept {
try {
  f();
  return nullptr;
} catch(const std::exception& e) {
  const char *s = strdup(e.what());
  return s == nullptr ? outOfMemory : s;
} catch(...) {
  return unknownError;
}
}

template<typename F>
void wrap_(F&& f) noexcept {
try {
  f();
} catch(...) {}
}

template<typename T>
void free_(T* obj) noexcept {
  wrap_([=] { delete obj; });
}

}
}
}
