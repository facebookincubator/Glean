<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/** Corge documentation */
function corge(inout int $mult, <<Attribute>> int $sub = 1, int ...$add): int {
  $result = 5000 * $mult;
  foreach ($add as $a) {
    $result += $a;
  }
  return $result - $sub;
}

const int WALDO = -3;

/** Position documentation */
enum Position: int as int {
  Top = 0;
  Bottom = 1;
  Left = 2;
  Right = 3;
  Center = 4;
}

enum UsingPosition: int {
  use Position;
  Hover = 5;
}

interface News {}
class Newspaper implements News {}
class Magazine extends Newspaper {}

enum class Extra: News {
  Newspaper Times = new Newspaper();
  Magazine Enonomist = new Magazine();
}

enum class ExtraExtra: News extends Extra {
  Magazine RollingStone = new Magazine();
}

/**
 * A doc comment for a typedef
 */
type FooType = shape(
  'bar' => bool,
  'baz' => ?string,
);

/* @lint-ignore-every TXT4 */
/* End file without newline char for FileLines predicate test */
