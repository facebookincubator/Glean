<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * Test Code Block
 */
<<TestAnnotation("Text")>>
class SourceClass extends SuperClass {

  const int BAZ = 2;

  const type T = int;

  public int $daz = 99;

  public function bar(int $param): int {
    return $param * corge();
  }

  /**
   * A thing that can be overridden
   */
  public abstract static function pleaseOverride(): string;
}

class SubClass extends SourceClass {
  <<__Override>>
  public static function pleaseOverride(): string {
    return 'override this';
  }
}

class TestAnnotation implements HH\ClassAttribute {
  public function __construct(private string $test) {}
}
