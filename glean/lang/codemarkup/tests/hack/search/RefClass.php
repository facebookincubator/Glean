<?hh
// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

class RefClass implements SourceInterface {
  use SourceTrait;

  const int JAZZ = 39;

  private int $raz = -50;

  public static function foo(): int {
    return corge() * WALDO;
  }

  public function bazza (): int {
    return 5 * corge();
  }

  public function bar(SourceClass::T $param): int {
    $source = new SourceClass();
    $result1 = corge() + SourceClass::BAZ + $source->daz + $this::JAZZ + WALDO;
    $result2 = $this->quux($result1) + Position::Right + $this::foo();
    $result3 = $this->raz + $this->bazza() + SourceClass::superAdd(1, $param);
    return ($result1 + $result2 + $result3 + Position::Center) * corge();
  }
}
