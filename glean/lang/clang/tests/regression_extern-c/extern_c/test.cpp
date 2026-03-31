/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

 
 // Test extern "C" declarations are tracked by ExternCDeclaration predicate.

extern "C" {

  void extern_c_function();
  int extern_c_variable;
  struct ExternCStruct {
    int x;
  };
  enum ExternCEnum {
    EC_VALUE_A,
    EC_VALUE_B,
  };
  typedef int ExternCTypedef;
}

// Second extern "C" block with more declarations. and an inner extern "C++" block.
extern "C" {

  void extern_c_function_2();
  int extern_c_variable_2;
  struct ExternCStruct2 {
    int x;
  };
  enum ExternCEnum2 {
    EC2_VALUE_A,
    EC2_VALUE_B,
  };
  typedef int ExternCTypedef2;

  // Nested extern "C++" inside extern "C" should NOT produce facts.
  extern "C++" {

    void not_extern_c_function();
    int not_extern_c_variable;
    struct NotExternCStruct {
      int x;
    };
    enum NotExternCEnum {
      NEC_VALUE_A,
      NEC_VALUE_B,
    };
    typedef int NotExternCTypedef;
  }
}

// Direct extern "C" declarations (not in a block).
extern "C" void direct_extern_c_function();
extern "C" int direct_extern_c_variable;
extern "C" typedef int DirectExternCTypedef;
extern "C" struct DirectExternCStruct { int y; };
extern "C" enum DirectExternCEnum { DEC_VALUE_A, DEC_VALUE_B };

// Regular (non-extern-C) declarations should NOT produce facts.
void regular_function();
int regular_variable;
struct RegularStruct {
  int z;
};
enum RegularCEnum {
    REG_VALUE_A,
    REG_VALUE_B,
  };
typedef int RegularCTypedef;
