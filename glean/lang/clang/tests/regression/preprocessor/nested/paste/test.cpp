/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAT(a, b) a##b
#define XCAT(a, b) CAT(a, b)
#define FOO_1 one
#define one 1
#define PICK(n) XCAT(FOO_, n)
int a = PICK(1);
int b = CAT(FOO_, 1);
int c = PICK(1);
