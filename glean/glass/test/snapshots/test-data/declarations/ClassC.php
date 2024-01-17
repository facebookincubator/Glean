<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @lint-ignore TXT2 */
/** Include tab 	 for testing purposes */
<<Oncalls('test_oncall1', 'test_oncall2')>>
final class ClassC extends ClassB {
  use TraitA;

  const int BAZ = 102;

  private string $fred = "fred";

  <<__Override>>
  public static function foo(int $a, ...): int {
    return 2;
  }

  <<__Override>>
  function testoverride(): string {
    return "hello";
  }

  <<__Deprecated("Do not use")>>
  protected function bar(this::T $param): string {
    return "Hello".$this->quux("baz").$param.$this->fred;
  }

  private function qux(): int {
    return 5;
  }

  private function many_types(
    Awaitable<?ClassA> $_i,
    Awaitable<Awaitable<?ClassB>> $_j,
    (int, ClassA, ~ClassB) $_k,
    dict<string, Awaitable<ClassA>, ClassA, ClassA, ClassA> $_l,
  ): (int, ClassA, ~ClassB, Awaitable<vec<ClassC>>) {
  }
}
