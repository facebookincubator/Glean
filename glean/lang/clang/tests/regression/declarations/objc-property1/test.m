/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface A {
  int _i;
}

@property (nonatomic, assign) int i;

@end

@implementation A

@synthesize i = _i;

@end
