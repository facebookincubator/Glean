/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/fact.h"
#include "glean/rts/id.h"
#include "glean/rts/lookup.h"
#include "glean/rts/substitution.h"
#include "glean/rts/bytecode/subroutine.h"
#include "glean/rts/bytecode/syscall.h"

#include <vector>

namespace facebook {
namespace glean {
namespace rts {

// NOTE: Any changes here should also be propagated to the internal.thrift
// types and serialize and deserialize should be updated accordingly.
//
// We aren't using the Thrift types here because it isn't clear if those will
// be needed in the long term and it's nice to be able to have methods.

/// Information about a predicate in an open DB.
struct Predicate {
  Pid id;
  std::string name;
  int32_t version;

  template<typename Context>
  using Rename = SysCalls<Context, SysCall<Id, Pid, Reg<Id>>>;
  template<typename Context>
  using Descend = SysCalls<Context, SysCall<Id, Pid>>;

  /// Typechecker for clauses. It should take the following arguments:
  ///
  /// std::function<Id(Id id, Id type)> - fact substitution
  /// const void * - begin of clause/key
  /// const void * - end of key/begin of value
  /// const void * - end of clause/value
  /// binary::Output * - substituted clause
  /// uint64_t * - size of substituted key
  std::shared_ptr<Subroutine> typechecker;

  /// Generic fact traversal. Takes these arguments:
  ///
  /// std::function<void(Id id, Pid type)> - called for each fact ID
  /// const void * - begin of clause/key
  /// const void * - end of key/begin of value
  /// const void * - end of clause/value
  std::shared_ptr<Subroutine> traverser;

  bool operator==(const Predicate& other) const;
  bool operator!=(const Predicate& other) const {
    return !(*this == other);
  }

  template<typename Context>
  void typecheck(
      const Rename<Context>& rename,
      Fact::Clause clause,
      binary::Output& output,
      uint64_t& key_size) const {
    runTypecheck(*typechecker, rename, clause, output, key_size);
  }

  template<typename Context>
  void substitute(
      const Rename<Context>& rename,
      Fact::Clause clause,
      binary::Output& output,
      uint64_t& key_size) const {
    // TODO: We implement substitution via the typechecker for now but it we
    // might want to generate a more efficient subroutine just for substitution.
    typecheck(rename, clause, output, key_size);
  }

  template<typename Context>
  static void runTypecheck(
      const Subroutine& sub,
      const Rename<Context>& rename,
      Fact::Clause clause,
      binary::Output& output,
      uint64_t& key_size) {
    Subroutine::Activation::with(
      sub,
      rename.handlers(),
      [&](auto& activation) {
        activation.run({
          reinterpret_cast<uint64_t>(clause.data),
          reinterpret_cast<uint64_t>(clause.data + clause.key_size),
          reinterpret_cast<uint64_t>(clause.data + clause.size()),
          reinterpret_cast<uint64_t>(&key_size)});
        output = std::move(activation.output(0));
    });
  }

  template<typename Context>
  void traverse(
      const Descend<Context>& descend,
      Fact::Clause clause) const {
    runTraverse(*traverser, descend, clause);
  }

  template<typename Context>
  static void runTraverse(
      const Subroutine& sub,
      const Descend<Context>& descend,
      Fact::Clause clause) {
    Subroutine::Activation::with(
      sub,
      descend.handlers(),
      [&](auto& activation) {
        activation.run({
          reinterpret_cast<uint64_t>(clause.data),
          reinterpret_cast<uint64_t>(clause.data + clause.key_size),
          reinterpret_cast<uint64_t>(clause.data + clause.size())});
    });
  }
};

/// Information about predicates in an open DB.
class Inventory {
public:
  Inventory();

  // The ids in 'predicates' are expected to be mostly dense (gaps are ok for
  // now but the Inventory will use O(max_id - min_id) space.
  explicit Inventory(std::vector<Predicate> predicates);

  const Predicate * FOLLY_NULLABLE lookupPredicate(Pid id) const &;

  Pid firstId() const {
    return first_id;
  }

  Pid firstFreeId() const {
    return firstId() + preds.size();
  }

  // TEMPORARY
  std::vector<const Predicate *> predicates() const;

  std::string serialize() const;
  static Inventory deserialize(folly::ByteRange);

  bool operator==(const Inventory& other) const {
    return first_id == other.first_id
      && preds == other.preds;
  }
  bool operator!=(const Inventory& other) const {
    return !(*this == other);
  }

private:
  Pid first_id;
  std::vector<Predicate> preds;
    // an INVALID Predicate::id means there is no predicate with that id
};

}
}
}
