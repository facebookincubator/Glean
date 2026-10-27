/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Indexed in cell fbcode; SIDL paths stay repo-relative, without the cell prefix Thrift paths get.

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/core/sidl/BindParams.sidl","name":"CellStruct","kind":"struct","line":9,"column":0,"endLine":16,"endColumn":1} */
struct CellStruct {
  int version;
};

/** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"CellEnum","kind":"enum","line":13,"column":0,"endLine":17,"endColumn":1} */
enum class CellEnum {
  /** Glean {"idl":"sidl","file":"xplat/arfx/osf/examples/echo/EchoService.sidl","name":"CellVal","kind":"enumerator","line":14,"column":2,"endLine":14,"endColumn":9} */
  CellVal = 0,
};
