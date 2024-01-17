<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace NS1 {
  /** Grault documentation */
  function grault() : string {
    return "cat";
  }

  type Complex = shape('real' => float, 'imag' => float);

  newtype Point = (float, float);
}
