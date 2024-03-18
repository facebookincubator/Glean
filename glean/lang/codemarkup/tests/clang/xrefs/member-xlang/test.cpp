/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * thrift file: thrift/compiler/test/fixtures/inheritance/src/module.thrift
 * thrift service: MyRoot
 * thrift function: bar
 */
void bar(int);

void baz() {
  bar(0);
}
