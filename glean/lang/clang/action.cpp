// Copyright (c) Facebook, Inc. and its affiliates.

#include "glean/lang/clang/action.h"

#if FACEBOOK
#include "glean/facebook/lang/clang/logger.h"
#include "common/fbwhoami/FbWhoAmI.h"
#endif

namespace facebook {
namespace glean {
namespace clangx {

ActionLogger::ActionLogger(const std::string& name,
                           const std::string& task,
                           const std::string& request,
                           const std::string& repo_name,
                           const std::string& repo_hash,
                           const uint32_t worker_index,
                           const std::string& origin,
                           const std::string& cwd_subdir,
                           bool log)
: logger(new Logger()) {
  enabled = log;
  if (enabled) {
    (*logger)
#if FACEBOOK
      .setClusterRegion(facebook::FbWhoAmI::getRegion())
#endif
      .setTask(task)
      .setRequest(request)
      .setRepo(repo_name)
      .setRevision(repo_hash)
      .setProcess(worker_index)
      .setCommand(name);
    if (!origin.empty()) {
      logger->setOrigin(origin);
    }
    if (!cwd_subdir.empty()) {
      logger->setSubdir(cwd_subdir);
    }
    start = Clock::now();
  }
}

ActionLogger::~ActionLogger() {
  if (enabled) {
    try {
      logger->setTimeElapsedMS(
        std::chrono::duration_cast<std::chrono::milliseconds>(
          Clock::now() - start).count());
      // need to set it to something, otherwise logger complains
      logger->setTimeElapsed(0);
      if (auto p = std::current_exception()) {
        auto wrapper = folly::exception_wrapper(p);
        (*logger)
          .setSuccess(false)
          .setError(wrapper.what().toStdString());
      } else {
        logger->setSuccess(true);
      }
      LOG_VIA_LOGGER_ASYNC(*logger);
    } catch(const std::exception& e) {
      LOG(ERROR) << "while logging to logger: " << e.what();
    } catch(...) {
      LOG(ERROR) << "unknown error while logging to logger";
    }
  }
}

bool ActionLogger::log_index(const SourceFile &source,
                             const FactStats &buf_stats,
                             const CacheStats &cache_stats,
                             std::function<bool()> &&callback) {
  (*logger)
    .setTarget(source.target)
    .setPlatform(source.platform)
    .setFile(source.file);
  bool ok = callback();
  (*logger)
    .setCompileError(!ok)
    .setFactBufferSize(buf_stats.memory)
    .setFactCacheSize(cache_stats.facts.memory);
  return ok;
}

void ActionLogger::log(std::function<void()> &&callback) {
  callback();
}

}
}
}
