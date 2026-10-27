/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package "meta.com/glean/test/clang/mixed"

struct GetNavigationRequest {
  1: string identifier;
}

enum CountOnlyMode {
  Disabled = 0,
  HitCount = 1,
}
