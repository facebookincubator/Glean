#pragma once

#include "glean/rts/inventory.h"
#include "glean/rts/lookup.h"

#include <limits>

namespace facebook {
namespace glean {
namespace rts {

struct Validate {
  bool typecheck = true;
  bool keys = true;
  size_t limit = std::numeric_limits<size_t>::max();
};

void validate(const Inventory& inventory, const Validate& val, Lookup& facts);

}
}
}
