---
id: incrementality
title: Incrementality
sidebar_label: Incrementality
---

This is a walkthrough of the most important parts of the
implementation of incrementality, aimed at people working on Glean.

## Units

A *unit* is an arbitrary string. In Thrift:

```
typedef string (hs.type = "ByteString") UnitName
```

Units do not need to be declared beforehand; a Unit exists if it is
the owner of at least one fact.

Each unit is assigned a unique integer, called `UnitId`. This is done
by the storage backend the first time the unit is encountered, and
thereafter the unit will be referred to by its `UnitId`.

## Capturing ownership data from the indexer

The indexer provides an initial mapping of facts to units (or units to
facts, if you like). A fact can have an arbitrary number of owners.
This is done in one of two ways.

### JSON

For indexers that produce JSON, the unit is an optional field:

```
[
{
   "predicate": "glean.test.Node.5",
   "facts" : [
       {
         "key": { "label": "d" }
       }
   ],
   "unit": "D"
},
```

The `unit` here is the owner of all the facts in the `facts` array.

For an example JSON file with units, see <SrcFile
file="glean/shell/tests/owner.glean" />.

### Binary

Indexers that produce binary facts can annotate a batch of facts with owners:

```
struct Batch {
  ...
  5: map<UnitName, list<Id> (hs.type = "VectorStorable")> (
    hs.type = "HashMap",
  ) owned;
```

Specifies ownership of the facts in this batch. The list is really a
list of intervals `[x1,x2, y1,y2, ... ]` representing the inclusive
ranges `x1..x2`, `y1..y2`, ... where `x1 <= x2`, `y1 <= y2`, ...

The ranges do not need to be sorted, and can overlap.

### Storing the ownership data

The raw ownership data from the indexer is captured when the facts are
written to the DB. The fact IDs in the ownership data are substituted
with the final fact IDs of the facts.

Note that we might be writing a fact that already exists in the DB; in
that case we don't write the fact, but we must still remember the
ownership mapping, because it might be adding additional owners.

## Ownership sets

In general the owner of a fact is an *ownership set*, which has the
following form:

```
  set ::= unit
        | set_1 || ... || set_n
        | set_1 && ... && set_n
```

That is, an ownership set is either a *unit*, or a disjunction of sets
(`set_1 || ... || set_n`), or a conjunction of sets (`set_1 && ... &&
set_n`).

Each unique set is assigned a unique ID, called `UsetId` in the
implementation. `UsetId` and `UnitId` use distinct ranges, so we can
represent a set in a canonical way as an operator (`||` or `&&`)
together with an integer set. The implementation of this is
`SetExpr<T>` in <SrcFile file="glean/rts/ownership/uset.h"/>.

A set may only refer to `UsetId`s that are smaller than its `UsetId`
(just like facts can only refer to smaller fact IDs).

The efficiency of this scheme depends on the assumption that while
there are a lot of facts, there are relatively few distinct ownership
sets. Typically we see between 10-100x more facts than sets. The
storage we need for sets is therefore small relative to the size of
the DB. This assumption rests on the indexer using a coarse enough
range of units; one unit per file or module is good, but one unit per
function would lead to more sets.

## Visibility and slices

When building an incremental DB, we will *exclude* some units from the
base DB. This is done by building a *slice* (see <SrcFile
file="glean/rts/ownership/slice.h"/>)

* A *slice* is a set of `UsetId`
* A `UsetId` is in the slice if
  * `unit`: `unit` is not excluded
  * `set_1 || ... || set_n`: any of `set_1 ... set_n` are in the slice
  * `set_1 && ... && set_n`: all of `set_1 ... set_n` are in the slice
* A fact is visible if its `UsetId` is in the slice

A slice is represented by a bitmap. To construct the slice we
determine the visibility of each `UsetId` in ascending order, so that
when we process a given set we already know the visibility of the sets
it refers to.

## Propagating ownership to dependent facts

The property we want is that

> In a DB consisting of all the visible facts for a given set of
  excluded units, every fact referenced by a visible fact is
  also visible.

(Basically, there are no dangling references.)

To achieve this, we have to make sure that the ownership sets are
built such that this property is true for any set of excluded units.

This is done as follows:

* for a fact F with ownership set A
  * for each fact referenced by F with ownership set B
    * we make the owner `A || B`

This is obviously correct: if F is visible, then every fact it refers
to will also be visible.

In practice we make this efficient by:

* Traverse facts in reverse order, so we always visit a fact before
  the facts it depends on, and we only have to visit each fact once.

* We only have disjunctions at this stage (conjunctions arise with
  derived facts in the next stage), so we can normalise `A || B` by
  taking the union of the sets `A` and `B`. We use an efficient
  integer set representation with fast union (see <SrcFile
  file="glean/rts/ownership/setu32.h"/>).

This propagation requires *O(facts)* time, and the current
implementation also requires *O(facts)* space too.

The implementation of this propagation is `computeOwnership` in <SrcFile file="glean/rts/ownership.cpp" />.

Ownership propagation happens after all the facts are written, but
before derivation starts. This is why we have the `glean complete`
command: it marks the predicates complete and computes ownership sets
for the written facts.

In the future we could consider

* Starting the propagation in parallel with fact writing. We may have to visit facts multiple times, however.
* Do it with less than *O(facts)* space, to support large DBs. We'll probably have to write intermediate results to the DB.

## Storing ownership in the DB

Ownership that we store in the DB consists of:

* `ownershipUnits`: a mappings from unit to `UnitId`
* `ownershipUnitIds`: a mapping from `UnitId` to unit. This is
  used only for introspection (displaying an ownership set, e.g. for
  the `:owner` command in the shell).
* `ownershipSets`: a mapping from `UsetId` to set, encoded using Elias Fano Coding.
* `factOwners`: a mapping from `Id` to `UsetId`, giving the owner for
 each fact ID. This is an interval map, so for a series of consecutive
 fact Ids with the same owner, we only store the first.

### Caching fact ownership

Reading from `factOwners` each time we want the ownership of a fact
can be very expensive, because we need to check the owner
every time we visit a fact during a query to determine its visibility.

We therefore cache the ownership information in memory. This is done
lazily: we read a page of fact owners at a time. To find the owner of
a fact, we binary-search in the page to find the beginning of the
interval containing the desired fact ID.

## Ownership for derived facts

> A derived fact should be visible if and only if all the facts that
  it was derived from are visible.

This naturally gives rise to the following implementation:

* The owner for a fact derived from facts `F1...Fn` with owners `O1...On`
  respectively is `O1 && ... && On`

This is straightforward except that we might have multiple threads
deriving in parallel, and e want to avoid having duplicate ownership
sets created.

So the implementation is similar to the way facts are written:

* When deriving we create a batch of ownership information
  (`DefineOwnership`) consisting of the new ownership sets and the
  mapping from facts to ownership sets. Each `DefineOwnership` is
  written to the DB as it is created, using `addDefineOwnership`.

* When writing the facts to the DB, we de-duplicate the ownership sets
  against those in the DB, and renumber the sets as necessary. This is
  `computeDerivedOwnership`.

Note that because derivation needs to know the owners of facts derived
from, the ownership propagation described above needs to be complete
first.

## Ownership for stacked databases

The main complication with stacked DBs is that a DB may induce new
ownership for facts in a lower DB in the stack, as described [above](#stacked-incremental-dbs).

For the stacked DB we do [ownership
propagation](#propagating-ownership-to-dependent-facts) as before,
including propagating ownership to facts in the base DBs. But we don't
want to have to consider all facts in the stack, so instead of walking
through all facts in the stack we switch to using a priority queue for
facts in the base DB(s) to track facts to which we are propagating
ownership.

Now that a fact in the base DB can have multiple owners, we have an
implementation choice:

1. `getOwner(fact)` returns *all* owners for the fact.
   * A fact is visible if *any* of its owners is in the slice.
   * When deriving facts later, we have to construct the set of the
disjunction of all owners for a fact we derived from.

2. `getOwner(fact)` returns the *first* owner in the stack. For this
to work, the first owner has to include the base DB owner too. For
example, if fact F has base owner A and new owner B propagated from
the stacked DB, then its owner in the stacked DB should be `A ||
B`. Now when deriving, we only have one owner for each fact and we
don't need to construct new sets.

In Glean we currently do (2) because it seemed simpler.

### Derived facts in stacked incremental DBs

There is a problem here: suppose we have a fact F with owner A in the
base DB, and we propagate a new owner B to F in a stacked DB. F's
owner is now `A || B`. However, facts that derived from F in the
original base DB will have ownership sets that don't include B.  The
consequence is that later we might have a situation where F is
visible, but facts derived from it are not.

To fix this, we want to do incremental derivation on the stacked DB,
and propagate this new ownership to the base DB, which will fix up the
ownership of the derived facts. Incremental derivation must therefore
consider facts that have new ownership in the stacked DB when
deriving. At the time of writing, this isn't implemented yet.
