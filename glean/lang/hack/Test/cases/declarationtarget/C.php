<?hh // strict
// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

class SourceC extends SourceB {
  <<__Override>>
  public function f(): void {
    TargetMethod::method();
  }
}
