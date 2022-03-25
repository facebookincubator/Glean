/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N1 {

namespace N2 {

namespace N3 { struct Foo {}; }

using N3::Foo;


// A function taking 'Foo x' and returning a pointer to a function that takes
// 'Foo y' and returns 'Foo';
Foo (*foo(Foo x))(Foo y);

struct Bar {
  // Same, except as a member
  Foo (*bar(Foo x))(Foo y);
};

}

}

using N1::N2::N3::Foo;

Foo id(Foo x);

// The 'Foo' in 'Foo x' should go through the using decl in N2, the other 'Foo'
// through the top level one
Foo (*N1::N2::foo(Foo x))(Foo y) {
  return &id;
}

Foo (*N1::N2::Bar::bar(Foo x))(Foo y) {
  return &id;
}
