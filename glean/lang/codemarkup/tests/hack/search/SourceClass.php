<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */


namespace NS;

type TopTierFnType = (function(int): Awaitable<int>);

class SourceClass extends SuperClass {

  const int BAZ = 2;

  const type T = int;

  type Complex = shape('real' => float, 'imag' => float);

  newtype Point = (float, float);

  public int $daz = 99;

  public function bar(int $param): int {
    return $param * corge();
  }
}
