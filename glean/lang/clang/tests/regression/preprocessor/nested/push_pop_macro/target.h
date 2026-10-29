/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define WRAP TYPE
using X = WRAP;
#pragma pop_macro("TYPE")
using Y = WRAP;
#pragma pop_macro("TYPE")
using Z = WRAP;
#pragma pop_macro("TYPE")
using W = WRAP;
