// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/cpp/glean.h"

#include <string>
#include <chrono>
#include <memory>
#include <functional>

#include <folly/Optional.h>

#if FACEBOOK
namespace facebook::logger {
  class GleanClangIndexerLogger;
}
#endif

namespace facebook {
namespace glean {
namespace clangx {

#if FACEBOOK
using Logger = facebook::logger::GleanClangIndexerLogger;
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
