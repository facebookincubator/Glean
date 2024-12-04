<?hh
// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

class SuperClass {

  const int BAZ = 999;

  public function superAdd(int $i, int $i): int {
    return $i + $i + 55;
  }
}
