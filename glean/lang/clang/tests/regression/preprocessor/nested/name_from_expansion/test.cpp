/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define f(x) x
#define F f
#define g(x) (x)
#define CALL(m) m(2)
int y = F(1);
int z = CALL(g);
int w = CALL(F);
