<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

abstract class ClassA {

  abstract const type T;

  abstract const int BAZ;

  /** Abstract bar documentation */
  protected abstract function bar(this::T $param): string;
}
