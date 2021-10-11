// Copyright (c) Facebook, Inc. and its affiliates.

include "glean/if/glean.thrift"

namespace hs Glean
namespace py3 glean
namespace rust glean
namespace cpp2 facebook.glean.thrift
namespace php glean

struct Revision {
  1: string hash;
}

// path relative to the repository root
typedef string FilePath

struct Diff {
  1: i32 line;
  // changes performed from this line
  2: i32 removed;
  // number of lines removed
  3: list<string> added;
// new lines to be added
}

struct FileModified {
  1: list<Diff> diffs;
}

struct FileMoved {
  1: FilePath new_path;
  2: list<Diff> diffs;
}

struct FileDeleted {}

union FileChange {
  1: FileModified modified;
  2: FileMoved moved;
  3: FileDeleted deleted;
} (hs.nonempty)

struct IndexRequest {
  1: glean.Repo repo;
  2: Revision base;
  3: map<FilePath, FileChange> changes;
}

struct IndexResponse {
  1: glean.Repo repo;
}

exception NoIndexerAvailable {
  1: string message;
}

service GleanIndexingService extends glean.GleanService {
  // Trigger the creation of an incremental database based on file changes
  // Calls to glean.finalize are required to check when the database is ready
  // for use.
  IndexResponse index(1: IndexRequest request) throws (
    1: NoIndexerAvailable n,
    2: glean.Exception e,
  );
}
