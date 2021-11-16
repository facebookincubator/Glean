/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface I

- (int)f1;
- (int)f2: (int) x;
- (int)f3a: (int) x f3b: (int) y;
- (int)f4a: (int) x : (int) y;

@end;

@implementation I

- (int)f1 {
  return 8;
}

- (int)f2: (int) x {
  return [self f1];
}

- (int)f3a: (int) x f3b: (int) y {
  return [self f2: x];
}

- (int)f4a: (int) x : (int) y {
  return [self f3a:x f3b:y];
}

@end;
