/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

int foo(int x) {
  return 0;
}

int bar(int x) {
  return 1 + foo(x);
}

int main(int x, char** _unused) {
  return bar(x) *  2;
}
