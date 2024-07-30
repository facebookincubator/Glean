/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/** Glean {"file": "test.thrift", "name": "ResponseCard", "kind": "union" } */
class ResponseCard {
};

/** Glean {"file": "test.thrift", "name": "GetNavigationRequest", "kind": "struct" } */
class GetNavigationRequest {
};

/** Glean {"file": "test.thrift", "name": "GetNavigationException", "kind": "exception" } */
class GetNavigationException {
};

/** Glean {"file": "test.thrift", "name": "CountOnlyMode", "kind": "enum" } */
enum class CountOnlyMode {
  Disabled = 0,
  HitCount = 1,
  FileCount = 2,
};
