/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface I {
  int v1;
}
- (int)f1;
@end;

@implementation I {
  int v1;
}
- (int)f1 {
  return 0;
}
@end;
