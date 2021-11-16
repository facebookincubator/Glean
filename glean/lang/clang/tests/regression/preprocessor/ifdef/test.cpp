/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define IFDEF1
#define IFDEF2

#ifdef IFDEF1
int f1() {
  return 1;
}
#endif

#ifndef IFDEF2
#error "ERROR!"
#else
void f2() {
  return;
}
#endif
