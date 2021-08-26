// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <atomic>
#include <cinttypes>
#include <memory>
#include <string>
#include <vector>

namespace facebook {
namespace glean {
namespace interprocess {

struct Counters {
  using counter_t = std::atomic<uint64_t>;

  virtual ~Counters() {}
  virtual counter_t *counter(size_t index) = 0;
};

void countersSetup(const std::string& path, size_t count);

std::unique_ptr<Counters> counters(const std::string& path, size_t size);

}
}
}
