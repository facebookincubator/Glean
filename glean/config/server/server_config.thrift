// @generated SignedSource<<a5134b42688f426f255e6ac7fba017d9>>
// DO NOT EDIT THIS FILE MANUALLY!
// This file is a mechanical copy of the version in the configerator repo. To
// modify it, edit the copy in the configerator repo instead and copy it over by
// running the following in your fbcode directory:
//
// configerator-thrift-updater glean/server/server_config.thrift
// Copyright (c) Facebook, Inc. and its affiliates.

namespace hs Glean
namespace java.swift com.facebook.glean.server_config
namespace py glean.server_config
namespace py3 glean
namespace php glean
namespace cpp2 glean.server_config
namespace rust glean_server_config

typedef string RepoName
typedef i64 Seconds

typedef i64 DBVersion (hs.newtype)

typedef map<string, string> (hs.type = "HashMap") Properties

// A retention policy for local DBs, specifying when DBs get
// automatically deleted.  The default is to never delete any DBs.
struct Retention {
  // If set, DBs older than this value will be deleted.
  1: optional Seconds delete_if_older;

  // If set, incomplete DBs older than this value will be deleted.
  5: optional Seconds delete_incomplete_if_older;

  // Retain at least this many DBs. Overrides delete_if_older and
  // retain_at_most. Note: the value here specifies a number of
  // *complete* databases to retain; incomplete or failed databases
  // are not subject to this retention.
  2: optional i32 retain_at_least;

  // If set, and we have more than this many local DBs, the oldest
  // will be deleted.
  3: optional i32 retain_at_most;

  // If set, instead of deleting a DB immediately, we keep it in an
  // "expiring" state for this many seconds. In this state, the DB
  // can still be queried as normal, but it will not be suggested to
  // new clients, and it will be removed from the shard map shortly
  // before it is actually deleted.
  4: optional i32 expire_delay;

  // Only retain DBs with the following poperties
  7: Properties required_properties = {};

  // If true, DBs matching this retention policy will be kept open on
  // the server at all times. Otherwise, DBs are opened on first use
  // and closed according to the DatabaseClosePolicy.
  8: bool keep_open = false;
}

struct DatabaseRetentionPolicy {
  // Retention to use if not overridden by repos
  1: Retention default_retention;

  // Retention policies to use per repo name. If there are multiple
  // policies, DBs are retained according to the union of the
  // policies. This allows DB retention to be applied based on DB
  // properties: each retention policy can specify different
  // required_properties fields.
  3: map<RepoName, list<Retention>> by_repo = {};

  // DEPRECATED: Retention policy to use per repo name
  2: map<RepoName, Retention> repos = {};
}

// If restore is enabled, then repos are restored from backup,
// according to the DatabaseRetentionPolicy for the repo name.
// DatabaseBackupPolicy.location is the location to restore from.
struct DatabaseRestorePolicy {
  1: bool enabled = false;

  // if enabled is true, this is a blocklist
  // if enabled is false, this is an allowlist
  2: set<RepoName> override = [];
}

struct DatabaseClosePolicy {
  // Close any open DBs after this much idle time
  1: Seconds close_after = 1800;
}

struct Backup {
  // Location to restore from or backup to.
  1: string location = "manifold:glean_databases/nodes/dev-backup";

  // Delete backups this many seconds after uploading. 0 = never
  2: i32 delete_after_seconds = 2592000; // 30 days
}

struct DatabaseBackupPolicy {
  // Only DB names in this list will backup.
  4: set_RepoName_8170 allowed = [];

  // Default location to restore from or backup to, unless
  // overridden by an entry in repos below.
  3: string location = "manifold:glean_databases/nodes/dev-backup";

  // Backup policy to use per DB name
  // Server will backup DB names *not* in this list, but they will have no TTL
  5: map<RepoName, Backup> repos = {};
}

// Server will be serving just a subset of the glean dbs specified in the policy
// Glean dbs are sharded by db name, so the list bellow just represents
// a list of dbs to be served by this server
struct StaticShardsPolicy {
  1: set<RepoName> shards;
}

// Server will serve all known glean dbs
struct NoShardsPolicy {}

// Delegate the sharding allocation to ShardManager
struct ShardManagerPolicy {
  // The total number of shards, as specified in the ShardManager spec
  1: i32 nshards;

  // The Shard Manager service name
  2: string service_name;
  3: string default_domain_id = "default";
}

struct ShardManagerMostRecentPolicy {
  1: ShardManagerPolicy shard_manager_policy;

  // The domain id used to track the most recent db instances
  // All other db instances should use the default domain id
  2: string most_recent_domain_id;
}

union ShardingPolicy {
  1: StaticShardsPolicy static_assignment;
  2: NoShardsPolicy no_shards;

  // A sharding policy for a domain-less Shard Manager spec
  3: ShardManagerPolicy shard_manager;

  // Sharding for all dbs except the most recent instances for each db name
  4: ShardManagerMostRecentPolicy shard_manager_most_recent;
}

// Configeration for Glean Servers
struct Config {
  1: DatabaseRetentionPolicy retention;
  2: DatabaseRestorePolicy restore;
  3: DatabaseClosePolicy close;
  4: DatabaseBackupPolicy backup;

  // How often to run the janitor, which initiates backup, restore,
  // and deletion operations on databases.  If missing, the janitor
  // will never run and none of these operations will be performed.
  5: optional Seconds janitor_period;

  // How often to query for backups. Runs in the janitor so a
  // timespan less than janitor_period will query for backups every
  // janitor run.
  21: Seconds backup_list_sync_period = 300;

  // If the user doesn't suppliy a max_results, then this is what we
  // use. Note: setting this makes it impossible to do a query with
  // unlimited results (which is arguably a good thing).
  6: optional i64 default_max_results;

  // If the user doesn't suppliy a max_bytes, then this is what we
  // use. Note: setting this makes it impossible to do a query with
  // unlimited bytes (which is arguably a good thing).
  18: optional i64 default_max_bytes;

  // If the user doesn't supply a max_time_ms, then this is what we
  // use. Note: setting this makes it impossible to do a query with
  // unlimited time (which is arguably a good thing).
  20: optional i64 default_max_time_ms;

  // An allocation limit to protect the server from bugs and runaway
  // queries.
  7: optional i64 query_alloc_limit;

  // Max logs/s per method
  8: i32 logging_rate_limit = 50;

  // how long to keep to keep write results for (in seconds)
  9: i32 db_writes_keep = 1200;

  // how often to reap writes (in seconds)
  10: i32 db_writes_reap = 300;

  // Number of threads processing write requests from the queue
  11: i32 db_writer_threads = 48;

  // Write queue size limit in MB
  12: i32 db_write_queue_limit_mb = 10000;

  // whether and how often to save checkpoints for ptail-based writes
  13: i32 db_ptail_checkpoint_bytes = 2000000000;
  14: bool db_ptail_checkpoint_enabled = true;

  // size of shared rocksdb cache in MB (0 means don't have a shared cache)
  15: i32 db_rocksdb_cache_mb = 8000;

  // lookup cache size limit for each database in MB
  16: i32 db_lookup_cache_limit_mb = 1000;

  // What binary representation to use for newly created databases
  // (nothing means use latest supported).
  19: optional DBVersion db_create_version;

  // Disable completed dependencies check in stored predicates derivation
  // process.
  22: bool disable_predicate_dependency_checks = false;

  // Should we compact the DB when it is complete, and before it is backed up?
  // For production use this should normally be true.
  23: bool compact_on_completion = false;

  // DEPRECATED
  24: bool enable_schema_evolution = true;

  // The minimum age (in seconds) of a DB before we consider listing with
  // GleanService.listDatabases(). The idea here is that if we wait a
  // little while, there's a better chance that the DB will have propagated
  // to all the servers.
  25: Seconds min_db_age = 0;

  // Out of all available and restorable databases, this server may serve
  // only part of them and sharding policy descrbes how the server picks them
  26: ShardingPolicy sharding = {"no_shards": {}};

  // If true, queries will use schema_id instead of schema_version
  // to resolve the query. Specifically the server will use the
  // first one that exists in the following list:
  //   * schema_id from the UserQuery request
  //   * --schema-id flag passed to the server
  //   * glean.schema_id from the DB
  //   * the latest all.N in the current schema instance
  27: bool use_schema_id = true;

  // If true, enable RocksDB's cache_index_and_filter_blocks
  28: bool db_rocksdb_cache_index_and_filter_blocks = false;

  // If true, a query that specifies a non-existent SchemaId will be
  // rejected.  If false, the query is performed using the DB's
  // schema, which might result in a response that the client doesn't
  // understand, leading to e.g. a deserialization failure.
  29: bool strict_query_schema_id = false;

  // If available use the manifold cli client instead of the Haskell one.
  30: bool use_manifold_cli = true;

  // The maximum age of the remote db list in seconds, after which the
  // server will restart if the remote db list server is believed alive.
  31: Seconds max_remote_db_list_age = 600;

  // 32: deprecated

  // Timeout for restores, to avoid blocking further restores if one
  // restore operation gets stuck for some reason.
  33: optional Seconds restore_timeout;
}

// The following were automatically generated and may benefit from renaming.
typedef set<RepoName> (hs.type = "HashSet") set_RepoName_8170
