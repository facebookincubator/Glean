#pragma once

#include <string>
#include <folly/Format.h>

namespace facebook {
namespace glean {
namespace rts {

[[noreturn]] void raiseError(const std::string& msg);

template <class... Args>
[[noreturn]]
inline std::string error(folly::StringPiece fmt, Args&&... args) {
  raiseError(folly::sformat(fmt, std::forward<Args>(args)...));
}

}
}
}
