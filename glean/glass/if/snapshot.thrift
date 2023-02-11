/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "glean/glass/if/glass.thrift"

namespace hs Glean

// A precomputed DocumentSymbolListX query (query and result)
struct DocumentSymbolListXQuery {
  1: glass.DocumentSymbolsRequest request;
  2: glass.RequestOptions options;
  3: glass.DocumentSymbolListXResult result;
}

// A list of precomputed queries. This can be used by Glass to
// answer some queries in a context when a Glean DB is not available
// (e.g. diff navigation).
struct Snapshot {
  1: list<DocumentSymbolListXQuery> queries;
}
