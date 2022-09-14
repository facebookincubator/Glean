<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

class SourceB {
  // function here
  public function f(): void {
    new TargetClass();
    TargetMethod::method();
  }
}
