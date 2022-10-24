<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

enum SourceEnum : string {
  A = 'a';
  B = 'b';
}

enum SourceEnum2 : int as int {
  A_TAG = 0;
  B_TAG = 1;
  C_TAG = 3;
}

interface IAInterface {}

enum class SourceEnumClass3: IAInterface {}
