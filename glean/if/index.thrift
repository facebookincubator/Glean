// Copyright (c) Facebook, Inc. and its affiliates.

include "glean/if/glean.thrift"

namespace hs Glean

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

struct FileDeleted {
}

union FileChange {
  1: FileModified modified;
  2: FileMoved moved;
  3: FileDeleted deleted;
}

struct IndexRequest {
  1: glean.Repo repo;
  2: Revision base;
  3: map<FilePath, FileChange> changes;
}

struct IndexResponse {
  1: glean.Repo repo;
}

service GleanIndexingService extends glean.GleanService {
  // Create an incremental database based on file changes
  IndexResponse index(1: IndexRequest request);
}
