/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The SIDL files are not indexed, so declarations come from the annotations.

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/BindParams.sidl","name":"BindParams","kind":"struct","line":9,"column":0,"endLine":16,"endColumn":1} */
struct BindParams {
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/BindParams.sidl","name":"version","kind":"field","line":10,"column":2,"endLine":10,"endColumn":14} */
  int version;
};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"EchoEnum","kind":"enum","line":13,"column":0,"endLine":17,"endColumn":1} */
enum class EchoEnum {
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"EchoFirst","kind":"enumerator","line":14,"column":2,"endLine":14,"endColumn":11} */
  EchoFirst = 0,
  EchoSecond = 1,
};
