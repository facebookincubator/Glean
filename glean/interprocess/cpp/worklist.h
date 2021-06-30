// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <cinttypes>
#include <vector>

#include <folly/Optional.h>

namespace facebook {
namespace glean {
namespace worklist {

struct Counter {
  struct Value {
    // index of current work item
    uint32_t start;
    // index of first work item not in current work block
    uint32_t end;

    uint32_t size() const {
      return start < end ? end - start : 0;
    }

    bool empty() const {
      return start >= end;
    }

    bool operator==(const Value& other) const {
      return start == other.start && end == other.end;
    }

    bool operator!=(const Value& other) const {
      return !(*this == other);
    }
  };

  /// Returns the next work item to process or none if we are done
  virtual folly::Optional<Value> next() = 0;

  virtual ~Counter() = default;
};

/// A Counter which simply returns all items in the range [index,end[ and
/// then stops.
std::unique_ptr<Counter> serialCounter(size_t index, size_t end);

/// A work-stealing Counter.
std::unique_ptr<Counter> stealingCounter(
  const std::string& path,  // path a file initialised via stealingCounterSetup
  size_t worker_index,      // index of current worker
  size_t worker_count       // total number of workers
);

/// Setup a stealing counter file
void stealingCounterSetup(
  const std::string& path,
  const std::vector<Counter::Value>& values
);

}
}
}
