/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "test.h"
#define CAT(a, b) a##b
#define ONE 1
#define LOCAL OUTER(CAT(O, NE))

int a = OUTER(1);
int b = OUTER(2);
int c = LOCAL;
