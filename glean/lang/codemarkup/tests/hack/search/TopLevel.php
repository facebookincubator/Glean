<?hh
// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

const int WALDO = -3;

function corge() : int {
  return 5000 + WALDO;
}

enum Position: int as int {
  Top = 0;
  Bottom = 1;
  Left = 2;
  Right = 3;
  Center = 4;
}
