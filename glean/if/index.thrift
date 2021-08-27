// Copyright (c) Facebook, Inc. and its affiliates.

include "glean/if/glean.thrift"

struct Revision {
  1: string hash;
}

struct FileChanges {
  1: string path;
  // path relative to the repository root
  2: list<Diff> diffs;
}

struct Diff {
  1: i32 line;
  // changes performed from this line
  2: i32 removed;
  // number of lines removed
  3: list<string> added;
// new lines to be added
}

struct IndexRequest {
  1: glean.Repo repo;
  2: Revision base;
  3: list<FileChanges> changes;
}

struct IndexResponse {
  1: glean.Repo repo;
}

service GleanIndexingService extends glean.GleanService {
  // Create an incremental database based on file changes
  IndexResponse index(1: IndexRequest request);
}
