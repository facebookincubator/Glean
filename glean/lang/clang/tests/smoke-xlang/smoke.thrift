/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

const string SOME_CONST = "foo";

union SomeUnion {
  1: string union_field_0;
  2: string union_field_1;
}

struct SomeStruct {
  1: string struct_field_0;
  2: string struct_field_1;
}

exception SomeException {
  1: string exception_field;
}

enum SomeEnum {
  EnumValue0 = 0,
  EnumValue1 = 1,
  EnumValue2 = 2,
}

service SomeService {
  SomeStruct someFunction(1: SomeUnion request) throws (
    1: SomeException someException,
  );
}
