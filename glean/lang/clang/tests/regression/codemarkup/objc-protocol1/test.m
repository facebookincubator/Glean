/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface J
- (void)checkProtocol:(Protocol *)f;
@end

@protocol Foo
@end

void call(J *s)
{
  [s checkProtocol:@protocol(Foo)];
}
