/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// A copy of the generated types/Types.hpp, with members needing the OSF
// runtime left out; osf_stubs.h stands in for its headers.

#pragma once

#include "osf_stubs.h"

namespace glean::sidl {

struct Options;
struct Request;
class IEchoService;

/** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"Mode","kind":"enum","line":15,"column":0,"endLine":18,"endColumn":1} */
enum class Mode : int16_t {
  /** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"Off","kind":"enumerator","line":16,"column":2,"endLine":16,"endColumn":9} */
  Off = 0,
  /** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"On","kind":"enumerator","line":17,"column":2,"endLine":17,"endColumn":8} */
  On = 1
};

/** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"kLimit","kind":"const","line":11,"column":0,"endLine":11,"endColumn":23} */
inline constexpr int32_t kLimit = 7;

/** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"Millis","kind":"alias","line":13,"column":0,"endLine":13,"endColumn":22} */
using Millis = uint64_t;

/** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"Options","kind":"struct","line":21,"column":0,"endLine":25,"endColumn":1} */
/// Retry policy for a request.
struct Options {
  /** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"timeout","kind":"field","line":22,"column":2,"endLine":22,"endColumn":20} */
  alignas(8) ::glean::sidl::Millis timeout = {};
  /// Upper bound on retries.
  /** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"retries","kind":"field","line":24,"column":2,"endLine":24,"endColumn":19} */
  int32_t retries = {};
};

/** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"Request","kind":"struct","line":27,"column":0,"endLine":30,"endColumn":1} */
struct Request {
  /** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"mode","kind":"field","line":28,"column":2,"endLine":28,"endColumn":15} */
  ::glean::sidl::Mode mode = ::glean::sidl::Mode::Off;
  /** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"options","kind":"field","line":29,"column":2,"endLine":29,"endColumn":21} */
  alignas(8) ::glean::sidl::Options options = {};
};

/** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"IEchoService","kind":"interface","line":32,"column":0,"endLine":36,"endColumn":1} */
class IEchoService : public ::arfx::osf::IService {
 public:
  /** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"echo","kind":"method","line":33,"column":2,"endLine":33,"endColumn":29} */
  virtual ::arfx::osf::FutureStatusOr<int32_t> echoAsync(int32_t value) noexcept = 0;
  /** Glean {"idl":"sidl","file":"fbcode/glean/lang/codemarkup/tests/clang/xlang/sidl/test.sidl","name":"configure","kind":"method","line":35,"column":2,"endLine":35,"endColumn":40} */
  virtual ::arfx::osf::FutureStatusOr<::glean::sidl::Options> configureAsync(const ::glean::sidl::Request& request) noexcept = 0;
};

} // namespace glean::sidl
