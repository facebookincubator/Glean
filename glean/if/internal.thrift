// Copyright (c) Facebook, Inc. and its affiliates.

include "glean/config/server/server_config.thrift"
include "glean/if/glean.thrift"

namespace cpp2 facebook.glean.thrift.internal
namespace hs Glean

struct KeyIterator {
  1: i64 type;
  2: string key;
  3: i64 prefix_size;
  4: bool first;
}

struct SubroutineState {
  1: binary code;
  2: i64 entry;
  3: i64 inputs;
  4: list<i64> locals;
  5: list<string> literals;
}

struct QueryCont {
  1: list<KeyIterator> iters;
  2: list<string> outputs;
  3: SubroutineState sub;
  // 4: deprecated, do not use
  5: i64 pid;
  6: optional Subroutine traverse;
}

// Types for serialising/deserialising inventories. See comments in
// rts/inventory.h.

struct Subroutine {
  1: list<i64> code;
  2: i64 inputs;
  3: i64 outputs;
  4: i64 locals;
  5: list<i64> constants;
  6: list<string> literals;
}

struct Predicate {
  1: glean.Id id;
  2: glean.PredicateRef ref;
  3: binary signature;
  4: Subroutine typechecker;
  5: Subroutine traverser;
}

struct Inventory {
  1: list<Predicate> predicates;
}

// -----------------------------------------------------------------------------
// DB metadata

struct DatabaseBroken {
  1: string task;
  2: string reason;
}

union DatabaseIncomplete {
  1: glean.Tasks tasks;
}

struct DatabaseComplete {
  1: glean.PosixEpochTime time;
}

struct DatabaseFinalizing {
}

// The status of data being written into a DB
union Completeness {
  1: DatabaseIncomplete incomplete;
  3: DatabaseComplete complete;
  4: DatabaseBroken broken;
  5: DatabaseFinalizing finalizing;
} (hs.prefix = "", hs.nonempty)

// Information about a database stored by Glean.
struct Meta {
  // Database version
  1: server_config.DBVersion metaVersion;

  // When was the database created
  2: glean.PosixEpochTime metaCreated;

  // Completeness status
  3: Completeness metaCompleteness;

  // Backup status
  4: optional string metaBackup;

  // Arbitrary metadata about this DB. Properties prefixed by
  // "glean."  are reserved for use by Glean itself.
  5: glean.DatabaseProperties metaProperties;

  // What this DB depends on.
  6: optional glean.Dependencies metaDependencies;

  // Whether all facts for a predicate have already been inserted.
  7: list<glean.PredicateRef> metaCompletePredicates;
} (hs.prefix = "")
