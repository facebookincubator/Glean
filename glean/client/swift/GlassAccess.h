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

class GlassAccess : public IGlassAccess {
 public:
  explicit GlassAccess(const std::string& root);
  ~GlassAccess() override = default;

  folly::coro::Task<std::optional<protocol::LocationList>> usrToDefinition(
      const std::string usr,
      const std::optional<std::string> revision = std::nullopt) override;

 protected:
  folly::coro::Task<std::optional<protocol::LocationList>> handleUSR(
      const std::string usr,
      const std::optional<std::string> revision,
      std::string msg);
  folly::coro::Task<std::optional<protocol::LocationList>> handleUSRHash(
      const std::string usr,
      const std::optional<std::string> revision,
      std::string msg);
  protocol::LocationList convertUSRSymbolDefinitionToLocations(
      const ::glean::USRSymbolDefinition& definition) const;
  template <typename T>
  folly::coro::Task<std::optional<T>> runGlassMethod(
      const std::string method,
      std::string msg,
      folly::Function<folly::coro::Task<T>()> f);

  folly::coro::Task<void> warmUpConnection();
  std::unique_ptr<apache::thrift::Client<::glean::GlassService>> client;

 private:
  std::string getHgRoot();
  std::string hgRoot_;
};
