<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * Tests require constraints
 */

abstract class Machine {
  public function openDoors(): void {
    return;
  }
  public function closeDoors(): void {
    return;
  }
}

class SuperEngine {
}

class Pilot {
}

interface CoolStuff {
}

interface Fliers {
  public function fly(): bool;
}

trait Plane {
  require extends Machine;
  require implements Fliers;
  require class AirBus;
  require implements CoolStuff;

  public function takeOff(): bool {
    $this->openDoors();
    $this->closeDoors();
    return $this->fly();
  }
}

class AirBus extends Machine implements Fliers, CoolStuff {
  use Plane;

  public function fly(): bool {
    return true;
  }
}

interface Jet {
  require extends Machine;
  require extends SuperEngine;
}
