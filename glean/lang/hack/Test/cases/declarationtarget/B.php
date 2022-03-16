<?hh // strict
// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

class SourceB {
  public function f(): void {
    new TargetClass();
    TargetMethod::method();
  }
}
