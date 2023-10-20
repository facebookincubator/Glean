/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface I

@property (readonly) int i;

@end

@implementation I

- (int)i {
  return 0;
}

- (int)j {
  return [self i];
}

@end
