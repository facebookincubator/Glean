#pragma once

#include "glean/rts/densemap.h"
#include "glean/rts/factset.h"
#include "glean/rts/lookup.h"
#include "glean/rts/stats.h"
#include "glean/rts/store.h"

namespace rocksdb {
struct Cache;
struct Iterator;
}

namespace facebook{
namespace glean {
namespace rocks {

using rts::Id;
using rts::Pid;

using Cache = ::rocksdb::Cache;

std::shared_ptr<Cache> newCache(
  size_t capacity
);

struct Database;

/// A rocksdb container for storing facts
struct Container {
  virtual ~Container() {}

  /// Close the 'Container' - accessing it afterwards isn't allowed.
  virtual void close() noexcept = 0;

  /// Write a key/value pair to the Container metadata.
  virtual void writeData(folly::ByteRange key, folly::ByteRange value) = 0;

  /// Lookup a key in the Container metadata.
  virtual bool readData(
    folly::ByteRange key, std::function<void(folly::ByteRange)> f) = 0;

  /// Optimise the container for reading
  virtual void optimize() = 0;

  /// Backup the Container to the specified directory.
  virtual void backup(const std::string& path) = 0;

  /// Convert the Container to a full fact Database with the given
  /// representation version - accessing the original Container afterwards isn't
  /// allowed. If the database is being created, start is the starting fact id.
  virtual
    std::unique_ptr<Database> openDatabase(Id start, int32_t version) && = 0;
};

enum class Mode {
  ReadOnly = 0,
  ReadWrite = 1,
  Create = 2
};

std::unique_ptr<Container> open(
  const std::string& path,
  Mode mode,
  folly::Optional<std::shared_ptr<Cache>> cache);

/// A rocksdb-based fact database
struct Database : rts::Lookup {
  virtual Container& container() noexcept = 0;

  using PredicateStats = rts::DenseMap<Pid, rts::MemoryStats>;

  virtual PredicateStats stats() const = 0;

  virtual void commit(rts::FactSet& facts) = 0;
};

void restore(const std::string& target, const std::string& source);

}
}
}
