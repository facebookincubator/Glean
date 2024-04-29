/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "test.h"

/** Glean { "file": "test.thrift", "service":"TestService", "function": "thrift1"} */
void thrift1();

// test thrift annotations
void thriftCaller() {
  thrift1();
  thrift2();
}
