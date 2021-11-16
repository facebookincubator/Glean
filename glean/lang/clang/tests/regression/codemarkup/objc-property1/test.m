/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface I

- (int)foo;
- (void)setFoo: (int) x;

@end;

int call(I *x)
{
  x.foo = 5;
  x.foo += 6;
  return x.foo;
}
