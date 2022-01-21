/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface J
- (void)checkSelector:(SEL)selector;
@end

void bar(SEL selector)
{
  //no op
}

void call(J *s)
{
  // these should both get xrefs
  SEL f = @selector(foo);
  [s checkSelector:@selector(foo)];
}
