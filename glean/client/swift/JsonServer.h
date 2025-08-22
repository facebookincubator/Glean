/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/coro/AsyncScope.h>
#include <folly/coro/BlockingWait.h>
#include <folly/coro/Task.h>
#include <folly/dynamic.h>
#include <gtest/gtest_prod.h>
#include <atomic>
#include <condition_variable>
#include <iostream>
#include <memory>
#include <mutex>
#include <optional>
#include <queue>
#include "glean/client/swift/IGlassAccess.h"
#include "glean/client/swift/ScubaLogger.h"
#include "rfe/scubadata/ScubaData.h"

using RequestData = std::string;

// Thread-safe queue implementation
class ThreadSafeQueue {
 public:
  ThreadSafeQueue() = default;

  void push(RequestData&& item) {
    std::lock_guard<std::mutex> lock(mutex_);
    queue_.push(std::move(item));
    condition_.notify_one();
  }

  bool tryPop(RequestData& item) {
    std::lock_guard<std::mutex> lock(mutex_);
    if (queue_.empty()) {
      return false;
    }
    item = std::move(queue_.front());
    queue_.pop();
    return true;
  }

  bool waitAndPop(RequestData& item, std::chrono::milliseconds timeout) {
    std::unique_lock<std::mutex> lock(mutex_);
    if (condition_.wait_for(
            lock, timeout, [this] { return !queue_.empty(); })) {
      item = std::move(queue_.front());
      queue_.pop();
      return true;
    }
    return false;
  }

 private:
  std::queue<RequestData> queue_;
  std::mutex mutex_;
  std::condition_variable condition_;
};

class JsonServer {
 public:
  JsonServer();
  ~JsonServer() = default;
  JsonServer(const JsonServer&) = delete;
  JsonServer& operator=(const JsonServer&) = delete;
  JsonServer(JsonServer&&) = delete;
  JsonServer& operator=(JsonServer&&) = delete;

  void setGlassAccess(std::unique_ptr<IGlassAccess> glassAccess);
  void setScubaLogger(
      std::unique_ptr<facebook::glean::swift::ScubaLogger> scubaLogger);
  void start(std::istream& input = std::cin, std::ostream& output = std::cout);
  void stop();

 private:
  void processInput(std::istream& input);
  folly::coro::Task<void> processOutput(std::ostream& output);
  folly::coro::Task<void> processRequest(
      const std::string& requestStr,
      std::ostream& output);
  FRIEND_TEST(JsonServerTest, USRToDefinitionRequest);
  FRIEND_TEST(JsonServerTest, USRToDefinitionRequestNoResults);
  FRIEND_TEST(JsonServerTest, UnknownMethodRequest);
  FRIEND_TEST(JsonServerTest, InvalidJSONRequest);
  std::atomic<bool> running_;
  std::unique_ptr<IGlassAccess> glassAccess_;
  std::unique_ptr<facebook::glean::swift::ScubaLogger> scubaLogger_;
  std::unique_ptr<facebook::rfe::ScubaData> scubaData_;
  ThreadSafeQueue requestQueue_;
  std::unique_ptr<folly::coro::CancellableAsyncScope> asyncScope_;

  folly::coro::Task<void> handleUSRToDefinitionRequest(
      const folly::dynamic& id,
      const std::string& usr,
      const std::optional<std::string>& revision,
      const std::string& mode,
      const std::optional<int>& delay,
      std::ostream& output);
  void sendErrorResponse(
      const folly::dynamic& id,
      int code,
      const std::string& message,
      std::ostream& output,
      const std::optional<std::string>& method = std::nullopt,
      const std::optional<std::string>& usr = std::nullopt,
      const std::optional<facebook::glean::swift::USRType>& usrType =
          std::nullopt,
      int64_t duration = 0,
      const std::string& mode = "production");

  facebook::glean::swift::USRType determineUSRType(const std::string& usr);
};
