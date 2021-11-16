/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface I

- (int)f1;
- (id)f2: (int) x;
- (id)f3a: (int) x f3b: (int) y;
- (id)f4a: (int) x : (int) y;

@end;
