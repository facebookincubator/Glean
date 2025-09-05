/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/GlassAccessRemote.h"
#include <folly/coro/BlockingWait.h>
#include "servicerouter/client/cpp2/ServiceRouter.h"

using namespace facebook;

GlassAccessRemote::GlassAccessRemote(const std::string& root)
    : GlassAccess(root) {
  auto params = facebook::servicerouter::ClientParams().setProcessingTimeoutMs(
      std::chrono::milliseconds(10000));

  std::string connectionTier = "glean.glass";
  client =
      servicerouter::cpp2::getClientFactory()
          .getSRClientUnique<apache::thrift::Client<::glean::GlassService>>(
              connectionTier, params);

  // Warm up the connection after client is initialized
  folly::coro::blockingWait(warmUpConnection());
}
