/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/JsonServer.h"
#include <folly/json.h>

JsonServer::JsonServer() : running_(false), glassAccess_(nullptr) {}

void JsonServer::setGlassAccess(std::unique_ptr<IGlassAccess> glassAccess) {
  glassAccess_ = std::move(glassAccess);
}

void JsonServer::start(std::istream& input, std::ostream& output) {
  running_ = true;

  std::string line;
  while (running_ && std::getline(input, line)) {
    if (!line.empty()) {
      processRequest(line, output);
    }
  }
}

void JsonServer::stop() {
  running_ = false;
}

void JsonServer::processRequest(
    const std::string& requestStr,
    std::ostream& output) {
  try {
    auto request = folly::parseJson(requestStr);

    // Extract request fields
    auto id = request.getDefault("id", folly::dynamic::object);
    auto method = request.getDefault("method", "").asString();
    auto value = request.getDefault("value", "").asString();

    if (method == "USRToDefinition") {
      handleUSRToDefinitionRequest(id, value, output);
    } else {
      // Send error response for unknown method
      sendErrorResponse(id, -32601, "Method not found", output);
    }
  } catch (const std::exception& e) {
    // Send parse error response
    sendErrorResponse(
        folly::dynamic::object,
        -32700,
        "Parse error: " + std::string(e.what()),
        output);
  }
}

void JsonServer::handleUSRToDefinitionRequest(
    const folly::dynamic& id,
    const std::string& usr,
    std::ostream& output) {
  if (!glassAccess_) {
    sendErrorResponse(
        id, -32603, "Internal error: GlassAccess not initialized", output);
    return;
  }

  // Call GlassAccess to get definition locations
  auto locations = glassAccess_->usrToDefinition(usr);

  // Create response
  folly::dynamic response = folly::dynamic::object;
  response["id"] = id;

  if (locations.has_value()) {
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
  } else {
    // No definitions found, return empty array
    response["result"] = folly::dynamic::array;
  }

  // Send response
  output << folly::toJson(response) << "\n";
}

void JsonServer::sendErrorResponse(
    const folly::dynamic& id,
    int code,
    const std::string& message,
    std::ostream& output) {
  folly::dynamic response = folly::dynamic::object;
  response["id"] = id;

  folly::dynamic error = folly::dynamic::object;
  error["code"] = code;
  error["message"] = message;
  response["error"] = error;

  output << folly::toJson(response) << "\n";
}
