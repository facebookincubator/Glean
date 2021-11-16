/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/ffi/wrap.h"
#include "glean/rts/fact.h"
#include "glean/rts/lookup.h"

#include <ostream>

using namespace facebook::glean;
using namespace facebook::glean::rts;

namespace facebook {
namespace glean {
namespace rts {

std::ostream& operator<<(std::ostream& os, Id id) {
  return os << '{' << id.toWord() << '}';
}

std::ostream& operator<<(std::ostream& os, Pid id) {
  return os << '{' << id.toWord() << '}';
}

}
}
}

namespace {

void checkLookupInvariants(Lookup& lookup) {
  const auto start = lookup.startingId();
  const auto finish = lookup.firstFreeId();

  std::vector<Fact::unique_ptr> facts(finish - start);
  std::map<Pid, std::map<folly::ByteRange,std::pair<Id,folly::ByteRange>>> keys;

  /***********************************************************************
   * load all facts
   ***********************************************************************/
  Id expected_id = start;
  for (auto i = lookup.enumerate(); auto ref = i->get(); i->next()) {
    CHECK_GE(ref.id, start);
    CHECK_LT(ref.id, finish);
    // NOTE: This assumes it's a dense fact set
    CHECK_EQ(ref.id, expected_id);
    ++expected_id;

    auto fact = Fact::create(ref);

    auto [_, added] = keys[fact->type()].insert(
      {fact->key(), {fact->id(), fact->value()}});
    CHECK(added);

    facts[ref.id - start] = std::move(fact);
  }
  // NOTE: This assumes it's a dense fact set
  CHECK_EQ(expected_id, finish);

  /***********************************************************************
   * typeById and factById
   ***********************************************************************/
  for (const auto& fact : facts) {
    const auto ty = lookup.typeById(fact->id());
    CHECK_EQ(ty, fact->type());

    const auto found = lookup.factById(fact->id(), [&](auto ty, auto clause) {
      CHECK_EQ(ty, fact->type());
      CHECK_EQ(clause.key().str(), fact->key().str());
      CHECK_EQ(clause.value().str(), fact->value().str());
    });
    CHECK(found);
  }

  /***********************************************************************
   * facts outside of the range don't exist
   ***********************************************************************/
  CHECK_EQ(lookup.typeById(start-1), Pid::invalid());
  CHECK(!lookup.factById(start-1, [](auto,auto){}));
  CHECK_EQ(lookup.typeById(finish), Pid::invalid());
  CHECK(!lookup.factById(finish, [](auto,auto){}));

  /***********************************************************************
   * idByKey
   ***********************************************************************/
  for (const auto& p : keys) {
    for (const auto& q : p.second) {
      const auto id = lookup.idByKey(p.first, q.first);
      CHECK_EQ(id, q.second.first);
    }
  }

  /***********************************************************************
   * enumerate
   ***********************************************************************/
  for (auto id = start; id < finish; ++id) {
    CHECK_EQ(lookup.enumerate(id)->get().id, id);
  }
  CHECK_EQ(lookup.enumerate(start-1)->get().id, start);
  CHECK_EQ(lookup.enumerate(finish)->get().id, Id::invalid());
  CHECK_EQ(lookup.enumerate(finish+1)->get().id, Id::invalid());

  for (auto id = start+1; id < finish; ++id) {
    CHECK_EQ(lookup.enumerate(Id::invalid(), id)->get().id, start);
  }
  CHECK_EQ(lookup.enumerate(Id::invalid(), start)->get().id, Id::invalid());

  for (auto id = start; id < finish - 2; ++id) {
    auto iter = lookup.enumerate(id, id+2);
    CHECK_EQ(iter->get().id, id);
    iter->next();
    CHECK_EQ(iter->get().id, id+1);
    iter->next();
    CHECK_EQ(iter->get().id, Id::invalid());
  }

  // check that calling next without calling get beforehand works
  for (auto id = start; id < finish - 3; ++id) {
      auto iter = lookup.enumerate(id, id+3);
      iter->next();
      CHECK_EQ(iter->get().id, id+1);
      iter->next();
      iter->next();
      CHECK_EQ(iter->get().id, Id::invalid());
  }

  /***********************************************************************
   * enumerateBack
   ***********************************************************************/
  expected_id = finish;
  for (auto iter = lookup.enumerateBack();
        auto ref = iter->get();
        iter->next()) {
    // NOTE: This assumes it's a dense fact set
    CHECK_EQ(ref.id, expected_id-1);
    CHECK_GE(ref.id, start);
    --expected_id;
  }
  // NOTE: This assumes it's a dense fact set
  CHECK_EQ(expected_id, start);

  CHECK_EQ(lookup.enumerateBack(finish)->get().id, finish-1);
  CHECK_EQ(lookup.enumerateBack(start)->get().id, Id::invalid());

  for (auto id = start; id < finish; ++id) {
    CHECK_EQ(lookup.enumerateBack(Id::invalid(), id)->get().id, finish-1);
  }
  CHECK_EQ(lookup.enumerateBack(Id::invalid(),finish)->get().id, Id::invalid());

  for (auto id = start; id < finish - 1; ++id) {
    auto iter = lookup.enumerateBack(id+2,id);
    CHECK_EQ(iter->get().id, id+1);
    iter->next();
    CHECK_EQ(iter->get().id, id);
    iter->next();
    CHECK_EQ(iter->get().id, Id::invalid());
  }

// check that calling next without calling get beforehand works
  for (auto id = start; id < finish - 3; ++id) {
      auto iter = lookup.enumerateBack(id+3, id);
      iter->next();
      CHECK_EQ(iter->get().id, id+1);
      iter->next();
      iter->next();
      CHECK_EQ(iter->get().id, Id::invalid());
  }

  /***********************************************************************
   * seek
   ***********************************************************************/
  for (const auto& p : keys) {
    const auto type = p.first;
    auto s = p.second.begin();
    const auto e = p.second.end();
    for (auto iter = lookup.seek(type, {}, 0); auto ref = iter->get(); iter->next()) {
      CHECK(s != e);
      CHECK_EQ(ref.type, type);
      CHECK_EQ(ref.id, s->second.first);
      CHECK_EQ(ref.key().str(), s->first.str());
      CHECK_EQ(ref.value().str(), s->second.second.str());
      // check that resuming seeks works
      CHECK_EQ(lookup.seek(type, ref.key(), 0)->get().id, ref.id);
      ++s;
    }
    CHECK(s == e);
  }

  // TODO: prefix seeks
}

extern "C" {
const char *glean_check_lookup_invariants(Lookup *lookup) {
  return ffi::wrap([=] {
    checkLookupInvariants(*lookup);
  });
}
}

}
