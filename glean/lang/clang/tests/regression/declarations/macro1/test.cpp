/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define TYPE int
#define VAR_x TYPE x;
#define VAR(x) TYPE x;

namespace N1 {

VAR_x
VAR(y)

}

#define STRUCT_X struct X { VAR_x; VAR(y); };
#define STRUCT(x) struct x { VAR_x; VAR(y); };
#define STRUCTIFY(x,body) struct x body;

namespace N2 {

STRUCT_X
STRUCT(Y)
STRUCTIFY(Z, { VAR_x; VAR(y); int z; })

}

#define BEGIN_X struct X {
#define BEGIN(x) struct x {
#define END };


namespace N3 {

BEGIN_X int x; };
BEGIN(Y) int x; };

}

namespace N4 {

BEGIN_X int x; END
BEGIN(Y) int y; END

}
