/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * @testname declarations/objc-property1
 * @type interface A
 */
@interface A {
  /**
   * @testname declarations/objc-property1
   * @type A::_i
   */
  int _i;
}

/**
 * @testname declarations/objc-property1
 * @type property i
 */
@property (nonatomic, assign) int i;

@end

/**
 * @testname declarations/objc-property1
 * @type implementation A
 */
@implementation A

@synthesize i = _i;

@end
