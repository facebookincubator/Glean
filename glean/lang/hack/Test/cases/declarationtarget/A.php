<?hh // strict
// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

class SourceA {
  public function f(): void {
    new TargetClass();
    TargetMethod::method();
  }
}

class TargetClass {
}

class TargetMethod {
  static public function method(): void {
  }
}
