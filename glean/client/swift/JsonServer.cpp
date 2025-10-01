/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/JsonServer.h"
#include <folly/coro/Sleep.h>
#include <folly/coro/ViaIfAsync.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/json.h>
#include <chrono>
#include <thread>
#include "glean/client/swift/Clock.h"

JsonServer::JsonServer() : running_(false), glassAccess_(nullptr) {
  // Initialize ScubaLogger and ScubaData
  scubaLogger_ = std::make_unique<facebook::glean::swift::ScubaLogger>();
  scubaData_ = std::make_unique<facebook::rfe::ScubaData>("swift_glass_client");
  asyncScope_ = std::make_unique<folly::coro::CancellableAsyncScope>();
}

void JsonServer::setGlassAccess(std::unique_ptr<IGlassAccess> glassAccess) {
  glassAccess_ = std::move(glassAccess);
}

void JsonServer::setScubaLogger(
    std::unique_ptr<facebook::glean::swift::ScubaLogger> scubaLogger) {
  scubaLogger_ = std::move(scubaLogger);
}

void JsonServer::start(std::istream& input, std::ostream& output) {
  running_ = true;

  // Start input processing thread
  std::thread inputThread([this, &input]() { processInput(input); });

  // Start output request processing using multi-threaded executor
  asyncScope_->add(folly::coro::co_viaIfAsync(
      folly::getGlobalCPUExecutor(), processOutput(output)));

  // Wait for input thread to complete
  if (inputThread.joinable()) {
    inputThread.join();
  }

  // Wait for all async tasks to complete
  folly::coro::blockingWait(asyncScope_->cancelAndJoinAsync());
}

void JsonServer::stop() {
  running_ = false;
}

void JsonServer::processInput(std::istream& input) {
  std::string line;
  while (running_ && std::getline(input, line)) {
    if (line.empty()) {
      // Empty line signals termination
      break;
    }
    requestQueue_.push(std::move(line));
  }
  // Signal termination to the processing thread
  running_ = false;
}

folly::coro::Task<void> JsonServer::processOutput(std::ostream& output) {
  while (running_) {
    RequestData request;
    // Use waitAndPop with timeout to allow checking running_ periodically
    if (requestQueue_.waitAndPop(request, std::chrono::milliseconds(100))) {
      // Process each request concurrently on the executor
      asyncScope_->add(folly::coro::co_viaIfAsync(
          folly::getGlobalCPUExecutor(), processRequest(request, output)));
    }
    // If no request was available, the loop continues and checks running_ again
  }
  co_return;
}

folly::coro::Task<void> JsonServer::processRequest(
    const std::string& requestStr,
    std::ostream& output) {
  facebook::glean::swift::Clock clock;

  try {
    auto request = folly::parseJson(requestStr);

    // Extract request fields
    auto id = request.getDefault("id", folly::dynamic::object);
    auto method = request.getDefault("method", "").asString();
    auto value = request.getDefault("value", "").asString();
    auto mode = request.getDefault("mode", "production").asString();

    // Extract optional revision field
    std::optional<std::string> revision;
    if (request.count("revision") && !request["revision"].isNull()) {
      revision = request["revision"].asString();
    }

    // Extract optional delay field (only for test mode)
    std::optional<int> delay;
    if (mode == "test" && request.count("delay") &&
        !request["delay"].isNull()) {
      delay = request["delay"].asInt();
    }

    if (method == "USRToDefinition") {
      co_await handleUSRToDefinitionRequest(
          id, value, revision, mode, delay, output);
    } else {
      // Send error response for unknown method
      auto duration = clock.duration();
      sendErrorResponse(
          id,
          -32601,
          "Method not found",
          output,
          method,
          value,
          determineUSRType(value),
          duration,
          mode);
    }
  } catch (const std::exception& e) {
    // Send parse error response - no method/usr available due to parsing error
    auto duration = clock.duration();

    // Escape and truncate the input string for safe logging
    std::string safeInput = requestStr;

    // Replace single quotes with escaped version to prevent quote injection
    size_t pos = 0;
    while ((pos = safeInput.find("'", pos)) != std::string::npos) {
      safeInput.replace(pos, 1, "\\'");
      pos += 2; // Move past the escaped quote
    }

    // Truncate very long inputs to prevent log spam
    constexpr size_t MAX_INPUT_LENGTH = 500;
    if (safeInput.length() > MAX_INPUT_LENGTH) {
      safeInput = safeInput.substr(0, MAX_INPUT_LENGTH) + "...[truncated]";
    }

    sendErrorResponse(
        folly::dynamic::object,
        -32700,
        "Parse error: " + std::string(e.what()) + " on input: '" + safeInput +
            "'",
        output,
        std::nullopt,
        std::nullopt,
        std::nullopt,
        duration,
        "production");
  }
}

folly::coro::Task<void> JsonServer::handleUSRToDefinitionRequest(
    const folly::dynamic& id,
    const std::string& usr,
    const std::optional<std::string>& revision,
    const std::string& mode,
    const std::optional<int>& delay,
    std::ostream& output) {
  // Start timing for scuba logging
  facebook::glean::swift::Clock clock;

  if (!glassAccess_) {
    auto duration = clock.duration();
    sendErrorResponse(
        id,
        -32603,
        "Internal error: GlassAccess not initialized",
        output,
        "USRToDefinition",
        usr,
        determineUSRType(usr),
        duration,
        mode);
    co_return;
  }

  // Apply delay if specified in test mode
  if (mode == "test" && delay.has_value() && delay.value() > 0) {
    co_await folly::coro::sleep(std::chrono::milliseconds(delay.value()));
  }

  // Determine USR type and method
  facebook::glean::swift::USRType usrType = determineUSRType(usr);
  std::string method = (usrType == facebook::glean::swift::USRType::SWIFT)
      ? "usrToDefinition"
      : "clangUSRToDefinition";

  facebook::glean::swift::Status status =
      facebook::glean::swift::Status::FAILED;
  std::string error;

  try {
    // Call GlassAccess to get definition locations
    auto locations = co_await glassAccess_->usrToDefinition(usr, revision);

    // Create response
    folly::dynamic response = folly::dynamic::object;
    response["id"] = id;

    if (locations.has_value() && !locations->empty()) {
      // Convert protocol::Location objects to JSON
      folly::dynamic result = folly::dynamic::array;
      for (const auto& location : locations.value()) {
        folly::dynamic locationJson = folly::dynamic::object;
        locationJson["uri"] = location.uri;

        folly::dynamic range = folly::dynamic::object;
        folly::dynamic start = folly::dynamic::object;
        start["line"] = location.range.start.line;
        start["character"] = location.range.start.character;

        folly::dynamic end = folly::dynamic::object;
        end["line"] = location.range.end.line;
        end["character"] = location.range.end.character;

        range["start"] = start;
        range["end"] = end;
        locationJson["range"] = range;

        result.push_back(locationJson);
      }
      response["result"] = result;
      status = facebook::glean::swift::Status::SUCCESS;
    } else {
      // No definitions found, return empty array
      response["result"] = folly::dynamic::array;
      status = facebook::glean::swift::Status::NOT_FOUND;
    }

    // Calculate duration and log the request
    auto duration = clock.duration();

    // Use folly::toJson(response) to get the result string for logging
    std::string resultStr = folly::toJson(response["result"]);

    // Log the request to Scuba
    scubaLogger_->logRequest(
        scubaData_.get(),
        method,
        usr,
        usrType,
        resultStr,
        status,
        duration,
        error,
        std::nullopt,
        mode);

    // Send response
    output << folly::toJson(response) << "\n";
    output.flush(); // Ensure response is immediately sent

  } catch (const std::exception& e) {
    error = e.what();

    // Calculate duration and log the failed request
    auto duration = clock.duration();

    sendErrorResponse(
        id,
        -32603,
        "Internal error: " + error,
        output,
        method,
        usr,
        usrType,
        duration,
        mode);
  }
}

void JsonServer::sendErrorResponse(
    const folly::dynamic& id,
    int code,
    const std::string& message,
    std::ostream& output,
    const std::optional<std::string>& method,
    const std::optional<std::string>& usr,
    const std::optional<facebook::glean::swift::USRType>& usrType,
    int64_t duration,
    const std::string& mode) {
  folly::dynamic response = folly::dynamic::object;
  response["id"] = id;

  folly::dynamic error = folly::dynamic::object;
  error["code"] = code;
  error["message"] = message;
  response["error"] = error;

  output << folly::toJson(response) << "\n";
  output.flush(); // Ensure error response is immediately sent

  // Log error to Scuba
  if (scubaLogger_ && scubaData_) {
    scubaLogger_->logRequest(
        scubaData_.get(),
        method.value_or("unknown"),
        usr.value_or(""),
        usrType.value_or(facebook::glean::swift::USRType::UNKNOWN),
        "[]",
        facebook::glean::swift::Status::FAILED,
        duration,
        message,
        std::nullopt,
        mode);
  }
}

facebook::glean::swift::USRType JsonServer::determineUSRType(
    const std::string& usr) {
  if (usr.length() >= 2 && usr.substr(0, 2) == "s:") {
    return facebook::glean::swift::USRType::SWIFT;
  } else if (usr.length() >= 2 && usr.substr(0, 2) == "c:") {
    return facebook::glean::swift::USRType::CPP;
  } else {
    return facebook::glean::swift::USRType::UNKNOWN;
  }
}
