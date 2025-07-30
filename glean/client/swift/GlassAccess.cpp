/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/GlassAccess.h"
#include <folly/ExceptionString.h>
#include <folly/String.h>
#include <folly/coro/BlockingWait.h>
#include <glog/logging.h>
#include "servicerouter/client/cpp2/ServiceRouter.h"

using namespace facebook;
using apache::thrift::RpcOptions;

const auto GlassTimeoutMs = 900;

GlassAccess::GlassAccess() {
  auto params = facebook::servicerouter::ClientParams().setProcessingTimeoutMs(
      std::chrono::milliseconds(10000));

  std::string connectionTier = "glean.glass";
  client =
      servicerouter::cpp2::getClientFactory()
          .getSRClientUnique<apache::thrift::Client<::glean::GlassService>>(
              connectionTier, params);
}

std::optional<protocol::LocationList> GlassAccess::usrToDefinition(
    const std::string& usr) {
  std::string msg;

  // Create USRToDefinitionRequest
  ::glean::USRToDefinitionRequest request;
  request.usr_ref() = usr;
  request.repo_name_ref() = "fbsource";

  // Call the Glass service to get symbol definition
  const auto result = this->runGlassMethod<::glean::USRSymbolDefinition>(
      "usrToDefinition",
      msg,
      [&]() -> folly::coro::Task<::glean::USRSymbolDefinition> {
        ::glean::RequestOptions ro;
        RpcOptions rpcOptions;
        rpcOptions.setTimeout(std::chrono::milliseconds(GlassTimeoutMs));
        co_return co_await client->co_usrToDefinition(rpcOptions, request, ro);
      });

  if (!result.has_value()) {
    return std::nullopt;
  }

  // Convert Glass result to protocol::LocationList
  protocol::LocationList locations;
  const auto& definition = result.value();

  // Extract location from USRSymbolDefinition
  const auto& gleanLocation = definition.location_ref().value();

  // Convert glean::LocationRange to protocol::Location
  // Access the range fields directly
  const auto& range = gleanLocation.range_ref().value();

  protocol::Position start(
      static_cast<int>(range.lineBegin_ref().value()),
      static_cast<int>(range.columnBegin_ref().value()));
  protocol::Position end(
      static_cast<int>(range.lineEnd_ref().value()),
      static_cast<int>(range.columnEnd_ref().value()));
  protocol::Range protocolRange(start, end);

  // Create URI from repository and filepath using URI escaping
  std::string filepath = gleanLocation.filepath_ref().value();
  std::string uri = "file://" + folly::uriEscape<std::string>(filepath);
  protocol::Location location(uri, protocolRange);

  locations.push_back(location);

  return locations;
}

template <typename T>
std::optional<T> GlassAccess::runGlassMethod(
    const std::string& method,
    std::string& msg,
    folly::Function<folly::coro::Task<T>()> f) {
  try {
    return std::make_optional(folly::coro::blockingWait(f()));
  } catch (::glean::ServerException& ex) {
    msg += "::glean::ServerException(" + ex.message().value() + ");";
    LOG(ERROR) << "EXCEPTION searching " << msg << "\n";
  } catch (std::exception& ex) {
    msg += folly::exceptionStr(ex) + ";";
    LOG(ERROR) << "EXCEPTION running " << method << ":" << ex.what() << ":"
               << msg << "\n";
  }
  return {};
}
