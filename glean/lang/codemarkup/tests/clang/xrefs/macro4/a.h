/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define REF_NONMACRO N::foo()

#define REF_MACRO \
  int x = 42;      \
  (void)x;\
