/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define F void f() {}
#define G void g() { f(); }
#define H void h() { g(); }
#define M F G H

M
