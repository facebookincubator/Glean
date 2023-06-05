/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/cpp/glean.h"

#include <string>
#include <chrono>
#include <memory>
#include <functional>

#include <folly/Optional.h>

#if GLEAN_FACEBOOK
namespace facebook::logger {
  class GleanClangIndexerLogger;
}
#endif

namespace facebook {
namespace glean {
namespace clangx {

#if GLEAN_FACEBOOK
using Logger = facebook::logger::GleanClangIndexerLogger;
#else
class Logger {
public:
  Logger() { }
  Logger& setTask(const std::string& task) { return (*this); }
  Logger& setRequest(const std::string& task) { return (*this); }
  Logger& setRepo(const std::string& repo) { return (*this); }
  Logger& setRevision(const std::string& repo_hash) { return (*this); }
  Logger& setProcess(const uint32_t& worker_index) { return (*this); }
  Logger& setCommand(const std::string& name) { return (*this); }
  Logger& setOrigin(const std::string& origin) { return (*this); }
  Logger& setSubdir(const std::string& cwd_subdir) { return (*this); }
  Logger& setTimeElapsedMS(const long& x) { return (*this); }
  Logger& setTimeElapsed(const std::string& x) { return (*this); }
  Logger& setSuccess(bool suc) { return (*this); }
  Logger& setError(const std::string& what) { return (*this); }
  Logger& setTarget(const std::string& target) { return (*this); }
  Logger& setPlatform(const folly::Optional<std::string>& platform) { return (*this); }
  Logger& setFile(const std::string& file) { return (*this); }
  Logger& setCompileError(bool err) { return (*this); }
  Logger& setFactBufferSize(bool sz) { return (*this); }
  Logger& setFactCacheSize(bool sz) { return (*this); }
};
#endif

struct SourceFile {
  std::string target;
  folly::Optional<std::string> platform;
  std::string dir;
  std::string file;
};

/**
 * A helper class to log indexing actions.
 *
 * Allows separating logging code that requires RTTI, from code inheriting
 * Clang classes which are normally built without RTTI.
 */
struct ActionLogger {
  using Clock = std::chrono::steady_clock;
  using FactStats = facebook::glean::cpp::FactStats;
  using CacheStats = facebook::glean::cpp::BatchBase::CacheStats;

  explicit ActionLogger(const std::string& name,
                        const std::string& task,
                        const std::string& request,
                        const std::string& repo_name,
                        const std::string& repo_hash,
                        const uint32_t worker_index,
                        const std::string& origin,
                        const std::string& cwd_subdir,
                        bool log = true);
  ~ActionLogger();

  bool log_index(const SourceFile &source,
                 const FactStats &buf_stats,
                 const CacheStats &cache_stats,
                 std::function<bool()> &&callback);
  void log(std::function<void()> &&callback);

private:
  bool enabled;
  Clock::time_point start;
  std::unique_ptr<Logger> logger;
};

}
}
}
