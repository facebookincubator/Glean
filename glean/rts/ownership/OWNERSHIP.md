# Glean Ownership System

## Overview

Every fact in a Glean database has an **owner** — a set expression describing
which source-code units (typically files) contributed to that fact's existence.
At query time, a fact is **visible** if and only if its owner set evaluates to
true under the current visibility policy. This is the mechanism that powers
incremental indexing: when a file changes, only the facts it owns need to be
re-derived.

## Key types

### The shared ID namespace

Units and sets share a single `uint32_t` ID range:

```
 0 ─────────── firstSetId ─────────── next_uset_id
 │  UnitIds    │  UsetIds (promoted)  │
 └─────────────┴──────────────────────┘
```

- **UnitId** (`[0, firstSetId)`): leaf identifiers, one per ownership unit
  (e.g. a source file). Assigned during `fillOwnership`.
- **UsetId** (`[firstSetId, next_uset_id)`): identifiers for promoted set
  expressions. Assigned by `Usets::promote()` during `completeOwnership`.
- `firstSetId = max_unit + 1` where `max_unit` is the highest UnitId seen
  in the ownership data.

### SetU32

A compressed set of `uint32_t` values (roaring-bitmap style). Internally
divided into 64K-element blocks, each stored as one of:

- **Sparse**: sorted array of 16-bit offsets (small blocks)
- **Dense**: 8 KiB bitset (medium blocks)
- **Full**: no storage needed (every element present)

Supports union (`merge`), membership testing, iteration, size queries,
and serialization to Elias-Fano encoding.

See `setu32.h`.

### Uset

A ref-counted `SetExpr<SetU32>` with a memoized hash and an optional
persistent `UsetId`. The set expression is either `Or` (visible if **any**
member is visible) or `And` (visible if **all** members are visible).

Key fields and API:

- `exp`: the `SetExpr<SetU32>` payload — a `SetU32` plus a `SetOp` (`Or`/`And`).
- `refs`: manual reference count (`uint32_t`). Managed by `Usets::use()` /
  `Usets::drop()` — when refs reach 0 the `Uset` is deleted from the store.
- `id`: a `UsetId` assigned by `promote()`, or `INVALID_USET` if transient.
- `hash`: memoized hash of the set content, computed by `rehash()`. Used
  for deduplication in the `Usets` `F14FastSet`.
- `promoted()`: returns `true` if a `UsetId` has been assigned.

See `uset.h`.

### Usets

A deduplicating store for `Uset` objects using content-based hashing (via
`folly::F14FastSet`). Manages `Uset` lifetimes via reference counting:

- `use(uset)` / `drop(uset)`: increment/decrement refs. When refs hit 0
  the `Uset` is deleted.
- `promote(uset)`: assigns a persistent `UsetId` and bumps refs so the
  set outlives transient users.
- `merge(a, b)`: returns the union of two sets, deduplicating via `add()`.

See `uset.h`.

### UsetsMerge

A deferred, balanced merge engine used during `completeOwnership`. Instead
of merging sets one-by-one (O(n) sequential unions), sets are queued per
fact ID and merged all at once via a balanced binary-reduction tree. A
bounded two-semispace LRU cache (10K entries) memoizes pairwise merges.

See `uset.h`.

## Data flow: `computeOwnership`

`computeOwnership` (`ownership.cpp`) is the top-level entry point. It
orchestrates three phases:

### Phase 1: `fillOwnership`

Reads the raw (unit → fact-ID-ranges) mapping from an `OwnershipUnitIterator`
and builds a `TrieArray<Uset>` — a trie mapping each fact ID to the set of
units that directly own it. The trie allows sharing of common set prefixes.

Output: a `TrieArray<Uset>` and `firstSetId` (= max_unit + 1).

### Phase 2: `collectUsets`

Walks the trie and deduplicates its `Uset` nodes into a `Usets` store. After
this, every distinct set has exactly one canonical `Uset*` in the store.

Output: a `Usets` container.

### Phase 3: `completeOwnership`

Propagates ownership transitively through fact references. This is the most
complex phase.

**Key invariant**: facts can only reference facts with *lower* IDs. By
iterating in reverse (highest ID first), we guarantee that when we visit
a fact, every higher-ID fact that references it has already propagated its
ownership set down.

For each fact F (visited in reverse order):

1. **Merge**: combine F's initial trie set with all sets queued by
   higher-ID referrers (via `UsetsMerge::addUsetAndMerge`).
2. **Promote**: assign a persistent `UsetId` to the merged set.
3. **Propagate**: for each fact R that F references (`R < F`), queue F's
   set for deferred merging into R (via `UsetsMerge::addUset`).

The deferred merge queue is bounded: when it exceeds 1 GB, all pending
merges are flushed eagerly (the "early merge" path).

For **stacked DBs**, referenced facts in the base DB (id < min_id) are
tracked in a sparse map. Their final set is OR'd with the base DB's
existing owner: `owner(F) = set || base_owner`.

After processing all local facts, a second pass propagates ownership
through base-DB facts using a priority queue (max-heap).

Output: the `Usets` store now contains all promoted sets, and a
`factOwners` interval map records each fact's `UsetId`.

## Ref counting (`use` / `drop`)

Every `Uset*` pointer that is "live" — stored in a data structure, held by
a cache entry, or actively being worked on — must have a corresponding ref.
`use(uset)` increments the ref count, `drop(uset)` decrements it; when refs
reach 0 the `Uset` is erased from the `Usets` store and deleted.

Who holds refs:

- **`UsetsMerge::refs`** (merge queue): `addUset(factId, uset)` calls
  `use(uset)` so the set stays alive while queued. The balanced-reduction
  `merge(a, b)` drops both inputs and the result gets a ref from the cache.
- **`UsetsMerge::Cache`**: `cache.insert(key, value)` calls `use()` on
  both key operands and the value. `evictOldCache()` calls `drop()` on
  all of them.
- **`Usets::idIndex_`**: `promote()` bumps refs by 1 so the set outlives
  all transient users and survives until the `Usets` container is destroyed.

Note: `facts` (the dense owner array) and `sparse` (the base-DB owner map)
also hold `Uset*` pointers, but their refs are NOT managed by `use()`/`drop()`.
Instead, refs are baked in during trie construction in `fillOwnership`: the
`TrieArray::insert` callback creates each `Uset` with `refs` equal to the
number of trie leaves (= facts) sharing it. These refs are carried through
`collectUsets` and into the flattened `facts` vector. When `processFact`
replaces an owner, it calls `drop()` on the old one — but this is a
convention, not enforced by the type system.

A typical lifecycle during `completeOwnership`:

```
1.  fillOwnership       →  Uset A created with refs = 1 (one trie leaf)
                           owner(F) = A  (no use() call — ref baked in by trie)
2.  processFact(G)      →  usetsMerge.addUset(F, set_G)    (use: refs(set_G) += 1)
3.  processFact(F)      →  merged = addUsetAndMerge(F, A)
      inside merge:        use(A), use(set_G)               (queue refs)
                           result = merge(A, set_G)          (drop A, drop set_G)
                           cache.insert(result)              (refs(result) = 1)
4.                      →  drop(A)                           (trie ref consumed)
5.                      →  promote(result)                   (refs(result) += 1, now = 2)
```

## Storage

The computed ownership is serialized and persisted by `storeOwnership`:

- Each promoted `Uset` is serialized to **Elias-Fano** encoding (a
  compact representation for monotone integer sequences) and stored
  in the `ownershipSets` column family, keyed by `UsetId`.

- The `factOwners` interval map is stored as a run-length-encoded
  sequence of `(fact-ID, UsetId)` pairs.

- Admin metadata (`AdminId` keys in `common.h`):
  - `FIRST_UNIT_ID`: start of this DB's allocation range.
  - `NEXT_UNIT_ID`: end of the allocated ID range (covers both units
    and promoted sets).

## Stacked (incremental) DBs

When a stacked DB is created on top of a base DB:

- `first_unit_id` is set to the base DB's `next_uset_id`, so the
  stacked DB's IDs don't collide with the base's.
- Ownership sets in the stacked DB may reference `UsetId`s from the
  base DB (e.g. `A || B` where `A` is a base-DB set).
- `getOwner()` traverses the stack downwards, returning the first
  owner found.

See `Note [stacked incremental DBs]` in `ownership.cpp`.

## File map

| File | Contents |
|------|----------|
| `ownership.cpp` | `fillOwnership`, `collectUsets`, `completeOwnership`, `computeOwnership` |
| `ownership/uset.h` | `Uset`, `Usets`, `UsetsMerge` |
| `ownership/uset.cpp` | `Usets::toEliasFano` |
| `ownership/setu32.h` | `SetU32` (compressed uint32 set) |
| `ownership/setu32.cpp` | `SetU32` implementation (merge, toEliasFano, iteration) |
| `ownership/triearray.h` | `TrieArray` (fact-ID → Uset mapping) |
| `storage/common.h` | `AdminId`, `DatabaseCommon`, storage helpers |
| `storage/db.h` | `Database` interface, `OwnershipSet`, ownership virtual methods |
