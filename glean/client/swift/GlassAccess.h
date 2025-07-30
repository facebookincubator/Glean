/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/coro/Task.h>
#include <memory>
#include "glean/client/swift/IGlassAccess.h"
#include "glean/glass/if/gen-cpp2/GlassServiceAsyncClient.h"
#include "glean/glass/if/gen-cpp2/glass_types.h"

class GlassAccess : public IGlassAccess {
 public:
  GlassAccess();
  ~GlassAccess() override = default;

  std::optional<protocol::LocationList> usrToDefinition(
      const std::string& usr) override;

 protected:
  template <typename T>
  std::optional<T> runGlassMethod(
      const std::string& method,
      std::string& msg,
      folly::Function<folly::coro::Task<T>()> f);

  std::unique_ptr<apache::thrift::Client<::glean::GlassService>> client;
};
