/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Test file to be used in Glass smoke test

#include "glean/lang/clang/tests/smoke-xlang/gen-cpp2/smoke_types.h"
#include "glean/lang/clang/tests/smoke-xlang/gen-cpp2/smoke_clients.h"
#include "glean/lang/clang/tests/smoke-xlang/gen-cpp2/smoke_constants.h"
#include <servicerouter/client/cpp2/ServiceRouter.h>

using namespace glean;

void thriftCaller() {
  SomeUnion request;
  SomeStruct response;
  SomeException e;
  e.exception_field().value();
  request.set_union_field_1("foo");
  auto client = facebook::servicerouter::cpp2::getClientFactory().getSRClientUnique<SomeService>("dummy.tier");
  client->sync_someFunction(response, request);
  response.struct_field_1().value();
  std::ignore = SomeEnum::EnumValue0;
  smoke_constants::SOME_CONST();
  std::ignore = smoke_constants::SOME_CONST_;
}
