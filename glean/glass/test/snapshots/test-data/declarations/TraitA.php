<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

trait TraitA {
  require extends ClassA;
  require implements InterfaceA;
  require class ClassA;

  /** Quux documentation */
  public function quux(string $s): string {
    return "string: ".$s;
  }

  abstract function testoverride(): string;
}
