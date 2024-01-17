<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @lint-ignore TXT5 */
/** Include multibyte UTF-8 char Я for testing purposes */
class ClassB extends ClassA implements InterfaceA {

  const type T = string;

  const int BAZ = 101;

  protected string $plugh = "plugh";

  /** Foo documentation */
  public static function foo (int $a, ...) : int {
      return 1 + $a;
  }

  /** Bar documentation */
  protected function bar(this::T $param, string $name="World"): string {
      return "Hello" . $param . $name;
  }

  private function qux () : int {
      return 7;
  }
}
