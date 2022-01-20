<?hh // strict
// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

class SourceClass extends SuperClass {

  const int BAZ = 2;

  const type T = int;

  public int $daz = 99;

  public function bar(int $param): int {
    return $param * corge();
  }
}
