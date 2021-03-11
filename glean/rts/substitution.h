#pragma once

#include <vector>

#include "glean/rts/id.h"

namespace facebook {
namespace glean {
namespace rts {

/**
 * Substitutions for blocks of consecutive Ids.
 *
 * A substitution implements a mapping Id -> Id for its range from
 * start() up to but not including finish(). Ids outside of this range are
 * mapped to themselves.
 *
 */
class Substitution {
public:
  Substitution(Id first, size_t size);

  Substitution(Id first, std::vector<Id> ids);

  Id subst(Id id) const {
    return id >= start() && id < finish()
      ? items[distance(start(),id)]
      : id;
  }

  Id start() const {
    return base;
  }

  Id finish() const {
    return base + items.size();
  }

  void set(Id pos, Id id) {
    CHECK(pos >= start() && pos < finish());
    items[distance(start(),pos)] = id;
  }

  void setAt(size_t index, Id id) {
    items[index] = id;
  }

  Id firstFreeId() const;

  static Substitution compose(
    const Substitution& first,
    const Substitution& second);

  bool operator==(const Substitution& other) const {
    return base == other.base && items == other.items;
  }

  bool operator!=(const Substitution& other) const {
    return !(*this == other);
  }

  thrift::Subst serialize() const;

  static Substitution deserialize(const thrift::Subst& subst);

  bool sanityCheck(bool incomplete) const;

private:
  Id base;
  std::vector<Id> items;
};

}
}
}
