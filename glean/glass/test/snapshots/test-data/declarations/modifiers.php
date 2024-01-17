<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Modifiers {

    public readonly function getData(readonly int $x): readonly int {
      return $x;
    }

    public function __construct() {}
  }

<<__EntryPoint>>
function example_readonly(): readonly int {
    return 0;
}
