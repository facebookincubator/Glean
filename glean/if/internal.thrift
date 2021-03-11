include "glean/if/glean.thrift"

namespace cpp2 facebook.glean.thrift.internal
namespace hs Glean

struct KeyIterator {
  1: i64 type
  2: string key
  3: i64 prefix_size
  4: bool first
}

struct SubroutineState {
  1: binary code
  2: i64 entry
  3: i64 inputs
  4: list<i64> locals
  5: list<string> literals
}

struct QueryCont {
  1: list<KeyIterator> iters
  2: list<string> outputs
  3: SubroutineState sub
  // 4: deprecated, do not use
  5: i64 pid
  6: optional Subroutine traverse
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
