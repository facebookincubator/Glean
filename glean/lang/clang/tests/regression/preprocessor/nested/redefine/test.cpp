/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "m.h"
#define N 1
int a = M;
int a2 = M;
#undef N
#define N 2
int b = M;
#define L N
int c = L;
#undef N
#define N 3
int d = L;
