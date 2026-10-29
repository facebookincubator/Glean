/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "h.h"
#define V 1
#define W V
#define L W
#if __cplusplus >= 201103L
int a = 1;
#endif
#ifdef __clang__
int b = 2;
#endif
long s = __INT64_C_SUFFIX__;
long t = __INT64_C_SUFFIX__;
