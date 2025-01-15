/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace NsA {

  namespace NsB {
    namespace NsC {
      namespace NsD {
        int fooFun() {
          return 0;
        }
      }
    }
    class classBinNSB {
      class classCinNSB {
        int fooCInBinB () { return 1; }
      };
    };
  }

  class ClassB { int fooB() { return 2; };

    class ClassC {
      int fooC() { return 3; }

      class D {
        int fooD() { return 4; }
      };
    };

  };

}

namespace G_NS {
  namespace F_NS {
    namespace E_NS {
      namespace D_NS {
        namespace C_NS {
          namespace B_NS {
            class A_Class {
              int foo_in_a_class() { return 1; }
            };
          }
        }
      }
    }
  }
}

namespace D1_NS {
  namespace C1_NS {
    class B_Class_1 {
      class A_Class_1 {
        int foo_in_a_class_1() { return 1; }
      };
    };
  }
}
namespace NS_5 {
  namespace NS_4 {
    namespace NS_3 {
      class Class_2 {
        class Class_1 {
          int foo_in_a_class_1_depth_5() { return 1; }
        };
      };
    }
  }
}
namespace NS_6 {
  namespace NS_5 {
    namespace NS_4 {
      class Class_3 {
        class Class_2 {
          class Class_1 {
            int foo_in_a_class_depth_6() { return 1; }
          };
        };
      };
    }
  }
}
