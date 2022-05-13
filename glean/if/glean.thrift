/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "glean/github/if/fb303.thrift"
include "glean/config/recipes/recipes.thrift"
include "glean/config/server/server_config.thrift"

cpp_include "folly/FBString.h"

hs_include "glean/if/glean_include.hs"

namespace cpp2 facebook.glean.thrift
namespace java.swift com.facebook.glean
namespace php glean
namespace py glean.glean
namespace py3 glean
namespace rust glean

// -----------------------------------------------------------------------------
// Basic types

// Uniquely identifies a fact in a database
typedef i64 Id

const Id INVALID_ID = 0;
const Id FIRST_FREE_ID = 1024;

// Used to identify versions of a predicate (TODO: merge with schema.thrift)
typedef i32 Version

// An identifer is a string of [A-Za-z][A-Za-z0-9_.]*
// Identifiers cannot use certain reserved words (see schema validator)
typedef string Identifier

typedef Identifier PredicateName
typedef Identifier TypeName

// Time points
typedef i64 PosixEpochTime (hs.newtype)

// JSON strings are represented by ByteString in Haskell
typedef string (hs.type = "ByteString") json

// Identifies a Predicate by name and version
struct PredicateRef {
  1: PredicateName name;
  2: Version version;
}

// Identifies a type by name and version
struct TypeRef {
  1: TypeName name;
  2: Version version;
}

// The unit of fact ownership
typedef string (hs.type = "ByteString") UnitName

// -----------------------------------------------------------------------------
// Runtime types

// Values of type 'nat'
typedef i64 Nat (hs.newtype)

// Values of type 'byte'
typedef byte Byte (hs.newtype)

// -----------------------------------------------------------------------------
// Information about databases and their status

// Uniquely identifies a database
struct Repo {
  1: string name;
  2: string hash;
}

typedef string WorkHandle

// waiting to be picked up by a worker
struct ParcelWaiting {
  1: i32 retries;
}

// being executed by a worker under the given handle
struct ParcelRunning {
  1: i32 retries;
  2: string handle;
  3: string runner;
  4: map<string, string> progress = {};
}

// successfully finished
struct ParcelFinished {}

// The current state of a work parcel
union ParcelState {
  1: ParcelWaiting waiting;
  2: ParcelRunning running;
  3: ParcelFinished finished;
}

// waiting for dependencies to finish
struct TaskWaiting {}

struct TaskRunning {
  // ParcelState for parcels [0,1..n-1] where n is Recipe.parcels.
  1: list<ParcelState> (hs.type = "Vector") parcels;
}

struct TaskFinished {
  1: PosixEpochTime time;
}

union TaskState {
  1: TaskWaiting waiting;
  2: TaskRunning running;
  3: TaskFinished finished;
}

struct Task {
  1: recipes.Recipe recipe;
  2: TaskState state;
}

typedef map<recipes.TaskName, Task> (hs.type = "HashMap") Tasks

union ScribeStart {
  1: string start_time;
  // Time point to start reading (otherwise read all available data)

  2: string checkpoint;
// Checkpoint to start from
} (hs.nonempty)

struct SendJsonBatchOptions {
  1: bool no_base64_binary = false; // See UserQueryOptions
}

union PickScribeBucket {
  1: i32 bucket;
// use this specific bucket

// This is a union type to allow for more options in the future: we
// might have the server pick a bucket for us, for example.
} (hs.nonempty)

// How to populate a DB from a Scribe category
struct WriteFromScribe {
  1: WorkHandle writeHandle;
  // The handle is used for finalizing the DB later via workFinished

  2: string category;
  // Scribe category to read from

  3: optional ScribeStart start;
  // Where to start from. If missing, use all the data in the bucket.

  4: optional SendJsonBatchOptions options;

  5: optional PickScribeBucket bucket;
// how to choose a bucket; omit for no bucket
}

typedef map<string, string> (hs.type = "HashMap") DatabaseProperties

// A Stacked DB that views only a portion of the underlying DB
struct Pruned {
  1: Repo base;
  2: list<binary> units;
  3: bool exclude; // True => exclude the units, otherwise include
}

// Dependencies of a DB (to be extended)
union Dependencies {
  1: Repo stacked; // TODO remove?
  2: Pruned pruned;
} (hs.nonempty)

// -----------------------------------------------------------------------------
// Thrift API

// decodes to a term (or a sequence of terms?)
typedef binary (cpp.type = "folly::fbstring") Value

// Special value Fact 0 "" "" returned when nothing found
struct Fact {
  1: Id type; // 'type' is Id for the Predicate definition
  2: Value key; // key decodes to a term that matches keyType
  3: Value value; // value decodes to a term that matches valueType
}

// A collection of facts which can be written to a database.
struct Batch {
  // Id of the first fact in the batch if ids isn't supplied and the boundary
  // between the underlying database and the batch - any fact id >= firstId
  // will be assumed to refer to facts in the batch and will not be looked
  // up in the underlying database.
  1: Id firstId;

  // Number of facts in the batch.
  2: i64 count;

  // Facts encoded in an internal binary format. Facts may only refer to facts
  // which occur before them in this sequence and to facts in the underlying
  // database with ids below firstId. If ids isn't supplied, the facts here
  // are assumed to have sequential ids starting with firstId.
  3: binary (cpp.type = "folly::fbstring") facts;

  // If supplied, this list contains the ids for the facts in the batch. It
  // must satisfy the following conditions:
  //
  //   - length == count
  //   - all elements are >= firstId
  //   - all elements are unique
  //   - ids are reasonably dense (writing the batch to the db will use a
  //     data structure of size O(max id - firstId))
  4: optional list<i64> (hs.type = "VectorStorable") ids;

  // (optional for now)
  //
  // Specifies ownership of the facts in this batch. The list is
  // really a list of intervals [x1,x2, y1,y2, ... ] representing
  // the inclusive ranges x1..x2, y1..y2, ... where x1 <= x2, y1 <= y2, ...
  //
  // The ranges do not need to be sorted, and can overlap.
  //
  // A fact can have an arbitrary number of owners.
  //
  // Units do not need to be declared beforehand; a Unit exists if
  // it is the owner of at least one fact.
  5: map<UnitName, list<Id> (hs.type = "VectorStorable")> (
    hs.type = "HashMap",
  ) owned;
}

struct Subst {
  1: Id firstId;
  2: list<i64> (hs.type = "VectorStorable") ids;
}

struct Error {
  1: string message;
}

exception Exception {
  1: string message;
}

exception BadQuery {
  1: string message;
}

exception InvalidLocator {
  1: string message;
}

// Operation should be retried later after at least the given number
// of seconds.
exception Retry {
  1: double seconds;
}

exception UnknownDatabase {
  1: Repo repo;
}

exception InvalidDependency {
  1: Repo repo;
  2: Repo dependency;
  3: string reason;
}

exception UnknownBatchHandle {}

enum DatabaseStatus {
  // database is available and complete:
  Complete = 0,
  // database is currently being created. Can be queried, but results
  // may be incomplete:
  Incomplete = 1,
  // database is in the process of being restored, and cannot be queried:
  Restoring = 2,
  // database exists, but creation did not complete successfully:
  Broken = 3,
  // database does not exist locally, but can be restored from backup:
  Restorable = 4,
  // database is being finalized; no further writing is allowed, but
  // it may be queried. Creation of a stacked DB is not allowed until
  // the DB finishes finalizing and is in the Complete state.
  Finalizing = 5,
  // database or one of its dependencies are missing:
  Missing = 6,
} (hs.nounknown)

struct DatabaseBroken {
  1: string task;
  2: string reason;
}

struct Database {
  1: Repo repo;

  // deprecated: 2
  // deprecated: 3

  // The status of this database including dependencies
  4: DatabaseStatus status;

  // The backup location of the database, if it is backed up
  5: optional string location;

  // In POSIX seconds, since epoch. This is used to compare database
  // versions.
  6: PosixEpochTime created_since_epoch;

  // If set, the DB is due to be expired at the specified time.
  // Clients should switch to a newer version of the DB.
  7: optional PosixEpochTime expire_time;

  // Arbitrary metadata about this DB. Properties prefixed by
  // "glean."  are reserved for use by Glean itself.
  8: DatabaseProperties properties;

  // If the DB is complete, this is the time when the DB was
  // marked completed.
  9: optional PosixEpochTime completed;

  // If present, this is the time when the source data was read.
  // This should always be earlier than created time.
  10: optional PosixEpochTime repo_hash_time;

  // What this DB depends on.
  11: optional Dependencies dependencies;

  // If the DB is broken, this gives more details
  12: optional DatabaseBroken broken;
}

struct PredicateStats {
  1: i64 count;
  2: i64 size;
}

/** Batch writes **/

typedef string Handle

// A part of a computed batch that can be sent to the server
struct ComputedBatch {
  1: Repo repo;

  // If true, the server will remember the handle which can then be
  // passed to finishBatch to check that the write has completed and
  // obtain the substitution.
  3: bool remember = false;

  4: Batch batch;
}

struct BatchRetry {
  1: double seconds;
}

union SendResponse {
  1: Handle handle;
  2: BatchRetry retry;
} (hs.nonempty)

union FinishResponse {
  1: Subst subst;
  2: BatchRetry retry;
} (hs.nonempty)

struct FinalizeResponse {}

struct Worker {
  1: string name;
}

struct Success {}
struct Failure {
  1: string message;
}

union Outcome {
  1: Success success;
  2: Failure failure;
}

struct UserQueryCont {
  3: binary continuation;
  4: i64 nextId;
  5: i32 version; // internal continuation version
  6: i64 hash; // internal continuation hash
  7: optional binary returnType; // angle return type
  8: list<i64> pids; // pids to expand in the results
  9: map<i64, i64> evolutions; // mapped predicates in the query
}

enum QuerySyntax {
  JSON = 1, // JSON query syntax
  ANGLE = 2, // Glean's query language
} (hs.nounknown)

struct UserQueryOptions {
  1: bool no_base64_binary = false;
  // DEPRECATED
  2: bool expand_results = true;
  // DEPRECATED

  3: bool recursive = false;
  // If true, then the query will fetch all nested facts recursively.
  // Note: it's easy to accidentally fetch a *lot* of data this way.
  // Consider using expand_results=false with recursive=true.

  4: optional i64 max_results;
  // If set, do not return more than this many results.  If there
  // may be more results to return, results_cont is set in the
  // UserQueryResults struct.
  //
  // NOTE: if you don't set max_results, the server might impose a
  // default max_results value. You can override the default by
  // specifying one here.

  8: optional i64 max_bytes;
  // If set, do not return more than this many bytes.  If there
  // may be more results to return, results_cont is set in the
  // UserQueryResults struct.
  //
  // NOTE: if you don't set max_bytes, the server might impose a
  // default max_bytes value. You can override the default by
  // specifying one here.

  10: optional i64 max_time_ms;
  // If set, return partial results with a continuation if the
  // query is still running after this amount of time (in
  // milliseconds).
  //
  // NOTE: if you don't set max_time_ms, the server might impose a
  // default max_time_ms value. You can override the default by
  // specifying one here, although note that your query might time
  // out in the network layer if you specify a larger timeout than
  // the network request timeout.

  5: optional UserQueryCont continuation;
  // continue a previous query.

  // choose the syntax for the query
  7: QuerySyntax syntax = JSON;

  // derive facts of "stored" type, and store them in the database
  9: bool store_derived_facts = false;

  // populate the facts_searched field of UserQueryStats
  11: bool collect_facts_searched = false;

  // debugging options
  12: QueryDebugOptions debug;

  // do not send the results with the response.
  // Saves the server the work of encoding and sending the response
  // through the wire.
  13: bool omit_results = false;
}

struct QueryDebugOptions {
  // dump the Intermediate Representation (IR) of the query after
  // flattening and expansion of derived predicates.
  1: bool ir = false;

  // dump the compiled bytecode for the query
  2: bool bytecode = false;
}

# Encode results using Glean's internal binary representation
struct UserQueryEncodingBin {}

# Encode results as JSON
struct UserQueryEncodingJSON {
  1: bool expand_results = false;
  // If true, then when a query specifies fetching nested facts,
  // those facts will be expanded in-place in the result facts.
  // If false, the nested facts are returned in the nestedFacts field
  // of UserQueryResults.
  //
  // Note that using 'true' here (the default) can lead to a much
  // larger result if the same facts are referred to in multiple
  // places. However, it may be more convenient for the caller to
  // have a single data structure rather than having to glue together
  // the nested facts manually.
  //
  // Some clients (in particular Haskell) set expand_results=false
  // and do the stitching-together automatically, so the result
  // is a data structure with internal sharing of repeated facts.
  2: bool no_base64_binary = false;
// Set to true if your client does not base64-encode the Thrift
// "binary" type in JSON.  This is needed in the following cases:
// - If your client is Python. The Python Thrift implementation is
//   broken and doesn't base64-encode the binary type.
// - If you're writing JSON directly (instead of generating it
//   from the Thrift types). In that case dealing with strings
//   is easier if you don't have to use base64 encoding. This is
//   how queries via the Glean shell work, for example.
// However, note that if you use this option then non-UTF8 data
// in a binary type may not be returned correctly, or you may
// encounter errors.

}

# Encode results as Thrift Compact
struct UserQueryEncodingCompact {
  1: bool expand_results = false;
// See expand_results in UserQueryEncodingJSON
}

# How to encode query results
union UserQueryEncoding {
  1: UserQueryEncodingBin bin;
  2: UserQueryEncodingJSON json;
  3: UserQueryEncodingCompact compact;
}

struct DerivePredicateQuery {
  1: string predicate;
  // Name of stored predicate to be derived
  2: optional Version predicate_version;
  // If omitted, defaults to the schema version used by the DB.
  4: optional UserQueryClientInfo client_info;
  // Information about who is making the call
  5: optional DerivePredicateOptions options;
  // How to parallelise derivation
  6: optional ParallelDerivation parallel;
  // If supplied, then any unversioned predicates in the query are
  // resolved using this version of the "all" schema. Otherwise, they
  // are resolved to the latest version of the "all" schema.
  7: optional Version schema_version;
}

// Derivation can be parallelised by partitioning over the range of an
// another predicate.  For instance, if you have a derived predicate like
//
//   predicate P : { a : A, b : B }
//      stored ...
//
// you could parallelise the derivation by partitioning over the facts
// of either A or B.  e.g. if we pick A, then we would specify
//
//   outer_predicate = "A"
//   inner_query = "P { a = X }"
//
// ("X" is a magic variable that will be bound to the facts of A when
// the query is performed by the server)
//
// Derivation then works by partitioning the facts of A into chunks,
// and running the query in parallel on the chunks of A facts.  The
// size of chunks and the degree of parallelism are chosen by the
// server, but you can give a minimum chunk size by setting
// min_batch_size.
//
// Note that you could break things by specifying an inner_query that
// doesn't yield all the facts of the predicate. Don't do that.
struct ParallelDerivation {
  // The predicate to partition over, e.g. "src.File"
  1: string outer_predicate;

  // The query to derive the facts, e.g. "python.DeclarationUses { file = X }"
  // The magic variable "X" will be bound to each fact of the outer_predicate,
  // and the query will be performed in parallel on batches of outer_predicate
  // facts.
  2: string inner_query;

  // minimum number of outer_predicate facts processed in each batch
  // (default: 1).
  3: optional i64 min_batch_size;
}

# A predicate is derived through multiple queries. These options work per query.
struct DerivePredicateOptions {
  1: optional i64 max_results_per_query;
  // maximum number of results to be batched for writing
  2: optional i64 max_bytes_per_query;
  // maximum number of bytes to be batched for writing
  3: optional i64 max_time_ms_per_query;
// maximum amount of time executing each batch
}

exception NotAStoredPredicate {}

exception UnknownDerivationHandle {}

exception UnknownPredicate {}

exception PredicateAlreadyComplete {}

exception PredicateAlreadyBeingDerived {}

exception IncompleteDependencies {
  1: list<PredicateRef> incomplete;
}

union DerivationProgress {
  1: UserQueryStats ongoing;
  2: UserQueryStats complete;
} (hs.nonempty)

struct DerivationOngoing {
  1: UserQueryStats stats;
}

struct DerivationComplete {
  1: UserQueryStats stats;
}

union DerivationStatus {
  1: DerivationOngoing ongoing;
  2: DerivationComplete complete;
} (hs.nonempty)

struct UserQuery {
  1: string predicate;
  // Name of the predicate to query
  // (only necessary when using JSON query syntax)
  2: string (hs.type = "ByteString") query;
  // Query string; syntax specified by UserQueryOptions.syntax
  3: optional Version predicate_version;
  // If provided, and if the version requested is different from
  // the predicate version in the DB, the server will attempt to
  // translate the results into the desired format. If this isn't
  // possible, an Exception will be thrown.
  // If omitted, defaults to the latest version of this predicate
  // in the schema version.
  4: optional Version schema_version;
  // If supplied, then any unversioned predicates in the query are
  // resolved using this version of the "all" schema. Otherwise, they
  // are resolved to the latest version of the "all" schema.
  5: optional UserQueryOptions options;

  6: list<UserQueryEncoding> encodings = [];
  // Acceptable encodings for the results in order of preference. The server
  // guarantees to return one of these encodings or fail.
  7: optional UserQueryClientInfo client_info;
// Information about who is making the call
}

struct UserQueryStats {
  // 1: deprecated
  2: i64 num_facts;
  // the number of individual facts returned in the result
  3: i64 elapsed_ns;
  // elapsed time to serve the request
  4: i64 allocated_bytes;
  // bytes allocated by the server
  5: optional map<Id, i64> facts_searched;
  // number of facts of each predicate searched. Use getSchemaInfo
  // to map Id to PredicateRef.
  6: optional i64 compile_time_ns;
  // time to compile the query
  7: optional i64 bytecode_size;
  // size of the compiled bytecode
  8: optional i64 execute_time_ns;
  // time to execute the compiled query
  9: i64 result_count;
// the number of top-level facts in the result. Not counting nested facts.
}

# Results in Glean's internal binary representation
struct UserQueryResultsBin {
  1: UserQueryEncodingBin encoding;
  2: map<Id, Fact> facts;
  3: map<Id, Fact> nestedFacts;
}

# Results in JSON
struct UserQueryResultsJSON {
  1: UserQueryEncodingJSON encoding;
  2: list<json> facts;
  3: map<Id, json> nestedFacts;
}

# Results in Thrift Compact
struct UserQueryResultsCompact {
  1: UserQueryEncodingCompact encoding;
  2: list<binary> facts;
  3: map<Id, binary> nestedFacts;
}

# Encoded query results
union UserQueryEncodedResults {
  1: UserQueryResultsBin bin;
  2: UserQueryResultsJSON json;
  3: UserQueryResultsCompact compact;
}

struct UserQueryResults {
  1: list<json> facts;
  // DEPRECATED

  2: optional UserQueryStats stats;
  // Stats about the query

  3: map<Id, json> nestedFacts;
  // DEPRECATED

  5: optional UserQueryCont continuation;
  // Set if max_results was set in the UserQueryOptions, and
  // there were additional results not returned.

  6: list<string> diagnostics;
  // Diagnostics from the query engine that may help if your
  // query didn't return the expected results.

  7: UserQueryEncodedResults results;
  // Results.

  8: optional Handle handle;
  // When store_derived_facts=True, and there were facts to write,
  // the writes are queued and this field contains the Handle
  // to pass to finishBatch to poll for completion.

  9: optional string type;
// The inferred type of the query
}

struct FactQuery {
  1: Id id;
  2: optional Version predicate_version; // see UserQuery
  3: bool recursive = false;
}

struct UserQueryFacts {
  1: list<FactQuery> facts;
  // Note: in the case of userQueryFacts, the length of the 'facts'
  // list in the returned UserQueryResults is guaranteed to be the
  // same length as this list.
  3: optional Version schema_version; // see UserQuery
  4: optional UserQueryOptions options;
  5: list<UserQueryEncoding> encodings = [];
  // Acceptable encodings for the results in order of preference.
  6: optional UserQueryClientInfo client_info;
// Information about who is making the call
}

struct UserQueryClientInfo {
  1: string name;
  // Client identifier (eg. api-haskell, api-python, etc)
  2: optional string unixname;
  // User making the query
  3: string application;
// Name of program making the query.
}

struct ListDatabases {
  1: bool includeBackups = false;
// If true, also queries the backup server for the databases that
// are available to restore from backup. This will therefore take
// longer than just listing the local databases.
}

struct ListDatabasesResult {
  1: list<Database> databases;
}

struct GetDatabaseResult {
  1: Database database;
  2: optional Tasks tasks;
}

struct DeleteDatabaseResult {}

struct JsonFactBatch {
  1: PredicateRef predicate;
  2: list<json> facts;
  3: optional string unit;
// the unit that owns these facts, if known.
}

struct SendJsonBatch {
  1: list<JsonFactBatch> batches;
  2: optional SendJsonBatchOptions options;

  // If true, the server will remember the handle which can then be
  // passed to finishBatch to check that the write has completed and
  // obtain the substitution.
  3: bool remember = false;
}

struct SendJsonBatchResponse {
  1: Handle handle;
}

// How to fill a database
union KickOffFill {
  1: string recipes;
  // Use the recipe set with the given name

  2: WorkHandle writeHandle;
  // Create a taskless DB which can be written to with the given handle

  3: WriteFromScribe scribe;
// Read from a scribe category to fill the database
}

struct KickOff {
  1: Repo repo;
  // What DB to kick off

  2: optional KickOffFill fill;
  // How to fill the DB - nothing means that the name of the recipe set
  // is the name of the repo

  3: DatabaseProperties properties;
  // Arbitrary metadata about this DB. Properties prefixed by
  // "glean."  are reserved for use by Glean itself.

  4: optional Dependencies dependencies;
  // What this DB depends on.

  5: optional PosixEpochTime repo_hash_time;
// The timestamp of the repo hash of this db.
}

struct KickOffResponse {
  1: bool alreadyExists;
}

struct UpdatePropertiesResponse {}

# A chunk of work that can be executed
struct Work {
  1: Repo repo;
  // Repository

  2: string task;
  // Task name

  3: i32 parcelIndex;
  // Index of the work parcel within the task (cf. Recipe.parcels).
  // The first parcel is parcelIndex 0.

  4: i32 parcelCount;
  // Total number of work parcels in the task

  5: WorkHandle handle;
// Unique handle
}

struct GetWork {
  1: i32 timeout = 20;
  // How long to wait for work (in seconds)

  2: list<string> tasks;
  // Tasks we are interested in.  Each entry is a regex which is
  // matched against the available tasks. To match a task exactly, use
  // "^name$" and escape any special regex characters inside name.

  3: string runner;
  // Machine or task that will be executing the work (for display only)

  4: optional Work previous;
// Work we've done right before (for optimising work assignments)
}

struct WorkAvailable {
  1: Work work;
  // The work

  2: i32 attempt;
  // How often the parcel has been retried

  3: optional i32 heartbeat;
  // How often to ping the server via workHeartbeat (in seconds)

  4: DatabaseProperties properties;
// Properties of the database this work is for
}

struct WorkUnavailable {
  1: i32 pause;
// How long to wait before calling getWork again (in seconds)
}

union GetWorkResponse {
  1: WorkAvailable available;
  2: WorkUnavailable unavailable;
} (hs.nonempty)

exception AbortWork {
  1: string reason;
}

struct WorkCancelled {
  1: Work work;
  2: string reason;
}

struct WorkHeartbeat {
  1: Work work;
}

struct WorkFinished {
  1: Work work;
  2: Outcome outcome;
}

struct SchemaInfo {
  1: string (hs.type = "ByteString") schema;
  2: map<Id, PredicateRef> predicateIds;
}

struct FactIdRange {
  1: Id start;
  2: Id finish;
}

struct ValidateSchema {
  1: string (hs.type = "ByteString") schema;
}

struct PredicateStatsOpts {
  1: bool excludeBase = true;
}

struct CompletePredicatesResponse {}

service GleanService extends fb303.FacebookService {
  // DEPRECATED
  list<Id> getPredicates(1: Repo repo, 2: list<PredicateRef> predicates);

  // Get the schema of a database. NOTE: This will replace getPredicates
  // and getSchema.
  SchemaInfo getSchemaInfo(1: Repo repo);

  // Check that a schema is valid, throws an exception if not.  Used
  // to verify a schema against the server before making it the
  // default.
  void validateSchema(1: ValidateSchema v) throws (1: Exception e);

  // Send a batch of fact. See the comments on ComputedBatch.
  SendResponse sendBatch(1: ComputedBatch batch);

  // Get the substitution for the given handle (obtained via a previous
  // sendBatch) if no writes are outstanding for it. The server forgets the
  // handle after this operation.
  FinishResponse finishBatch(1: Handle handle) throws (1: UnknownBatchHandle e);

  // Write a batch of facts in JSON format. The call will queue the
  // writes and return immediately. If the caller sets remember=true,
  // then they can later request the result by passing the returned
  // handle to finishBatch.
  SendJsonBatchResponse sendJsonBatch(1: Repo repo, 2: SendJsonBatch s) throws (
    1: Exception e,
    2: Retry r,
  );

  // Kick off a database; does nothing if the DB already exists
  KickOffResponse kickOff(1: KickOff request) throws (
    1: UnknownDatabase u,
    2: InvalidDependency e,
  );

  // Add, update, or delete DatabaseProperties for the given Repo.
  UpdatePropertiesResponse updateProperties(
    1: Repo repo,
    2: DatabaseProperties set_ = {},
    3: list<string> unset = [],
  ) throws (1: Exception e, 2: UnknownDatabase u);

  // Get a work parcel
  GetWorkResponse getWork(1: GetWork request) throws (1: Exception e);

  // Tell the server that the work parcel is no longer being worked on. The
  // server will then reschedule it as appropriate. This does not count as a
  // retry.
  void workCancelled(1: WorkCancelled request) throws (
    1: Exception e,
    2: AbortWork a,
  );

  // Tell the server that work is still ongoing. This needs to be done
  // periodically as indicated by WorkAvailable.heartbeat.
  void workHeartbeat(1: WorkHeartbeat request) throws (
    1: Exception e,
    2: AbortWork a,
  );

  // Tell the server that work has finished, either successfully or
  // unsuccessfully. If this is the final task and the server still
  // has pending writes, it will fail with Retry.
  void workFinished(1: WorkFinished request) throws (
    1: Exception e,
    2: AbortWork a,
    3: Retry r,
  );

  // Tell the server when non-derived predicates are complete.  This
  // call must be completed successfully before deriveStored() is
  // called.
  //
  // Note that the process of completing predicates may take some
  // time, and the call may return Retry multiple times. You can't
  // call deriveStored() until completePredicates() has returned
  // successfully.
  CompletePredicatesResponse completePredicates(
    1: Repo repo,
  // later: 2: optional list<PredicateRef> predicates
  ) throws (1: Exception e, 3: Retry r);

  // Wait for a DB to be complete, after the last workFinished
  // call. If finalization failed, this will throw an Exception with
  // the failure reason. If finalization is still in progress, this
  // will throw Retry.
  FinalizeResponse finalize(1: Repo repo) throws (1: Exception e, 3: Retry r);

  // Return Fact 0 "" "" when nothing found
  Fact queryFact(1: Repo repo, 2: Id id);

  // Get lower and upper bounds on fact ids in the database. The database is
  // guaranteed to have no fact ids < start or >= finish and fact ids within
  // the range will be reasonably dense. There is no guarantee that they are
  // consecutive or that a fact with id start exists.
  FactIdRange factIdRange(1: Repo repo) throws (1: Exception e);

  // DEPRECATED
  Id firstFreeId(1: Repo repo) throws (1: Exception e);

  map<Id, PredicateStats> predicateStats(
    1: Repo repo,
    2: PredicateStatsOpts opts,
  ) throws (1: Exception e);

  ListDatabasesResult listDatabases(1: ListDatabases l);
  GetDatabaseResult getDatabase(1: Repo repo) throws (
    1: Exception e,
    2: UnknownDatabase u,
  );
  DeleteDatabaseResult deleteDatabase(1: Repo repo) throws (
    1: Exception e,
    2: UnknownDatabase u,
  );

  void restore(1: string locator) throws (1: InvalidLocator e);

  UserQueryResults userQueryFacts(1: Repo repo, 2: UserQueryFacts q) throws (
    1: Exception e,
    3: BadQuery b,
  );

  UserQueryResults userQuery(1: Repo repo, 2: UserQuery q) throws (
    1: Exception e,
    3: BadQuery b,
    4: Retry r,
  );

  DerivationStatus deriveStored(
    1: Repo repo,
    2: DerivePredicateQuery q,
  ) throws (
    1: Exception e,
    2: NotAStoredPredicate n,
    3: UnknownPredicate u,
    4: IncompleteDependencies d,
  );
}

struct PredicateAnnotation {
  1: PredicateName name;
  2: i32 version;
}
