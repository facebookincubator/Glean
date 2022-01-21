/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface AA {
  int _i;
}

- (int)meth1;
- (int)meth1: (int) x sel: (int) y;

@property (nonatomic, assign) int prop1;

@end

@implementation AA

@synthesize prop1 = _i;

- (int)meth1 { return 3; };
- (int)meth1: (int) x sel: (int) y { return x; };

@end

@interface AA (CC)
- (int)meth2: (int) x;
@end

@implementation AA (CC)

- (int)meth2: (int) x { return x; }

@end
