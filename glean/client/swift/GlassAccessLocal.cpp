/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/GlassAccessLocal.h"
#include <folly/coro/BlockingWait.h>
#include "c2p/secure_thrift/cpp/client/SecureThrift.h"

using namespace facebook;

GlassAccessLocal::GlassAccessLocal(const std::string& root)
    : GlassAccess(root) {
  std::string connectionTier = "glean.glass";
  client = facebook::corp2prod::client::getClientFactory()
               .getClientUnique<apache::thrift::Client<::glean::GlassService>>(
                   connectionTier);

  // Warm up the connection after client is initialized
  folly::coro::blockingWait(warmUpConnection());
}
