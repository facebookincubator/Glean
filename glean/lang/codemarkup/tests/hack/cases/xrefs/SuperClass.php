<?hh // strict
// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

class SuperClass {

  const int BAZ = 999;

  public function superAdd(int $i, int $i): int {
    return $i + $i + 55;
  }
}
