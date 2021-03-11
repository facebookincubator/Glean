#pragma once

#include "glean/rts/id.h"

#include <iostream>
#include <rapidcheck.h>

namespace rc {

template<typename T>
struct Arbitrary<facebook::glean::rts::WordId<T>> {
  static Gen<facebook::glean::rts::WordId<T>> arbitrary() {
    return gen::map(
      gen::inRange(
        facebook::glean::rts::WordId<T>::lowest().toWord(),
        typename facebook::glean::rts::WordId<T>::word_type(0xFFFFFF)),
      facebook::glean::rts::WordId<T>::fromWord);
  }
};

}

namespace facebook {
namespace glean {
namespace rts{

template<typename T>
std::ostream& operator<<(std::ostream& out, WordId<T> id) {
  return out << '$' << id.toWord();
}

}
}
}
