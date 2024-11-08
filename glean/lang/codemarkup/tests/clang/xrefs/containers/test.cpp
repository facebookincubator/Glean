/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace foo {

struct S { int n; };

union U
{
    int n;
    double c;
};

typedef int this_is_a_typedef;

using Ptr = int*;

enum E { e_a, e_b, e_c };

int this_is_a_function(int a, int b)
{
    int this_is_a_variable = a + b;
    return a>b?a:b;
}

}
