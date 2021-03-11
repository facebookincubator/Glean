#include "glean/rts/binary.h"

namespace facebook {
namespace glean {
namespace binary {

std::vector<unsigned char> lexicographicallyNext(folly::ByteRange range) {
  const auto end = std::make_reverse_iterator(range.begin());
  auto pos = std::make_reverse_iterator(range.end());
  while (pos != end && *pos == 0xFF) {
    ++pos;
  }
  std::vector<unsigned char> bytes(range.begin(), pos.base());
  if (!bytes.empty()) {
    ++bytes.back();
  }
  return bytes;
}

void Output::Buf::realloc(size_t n) {
  // Just grow by 2, but start with at least 64 - we typically have very small
  // or very large buffers.
  const auto wanted =
    std::max(capacity() + std::max(capacity(), n), size_t(64));
  const auto new_cap = folly::goodMallocSize(wanted);
  auto new_buf = ffi::malloc_array<unsigned char>(new_cap);
  if (len > 0) {
    std::memcpy(new_buf.get(), buf.get(), len);
  }
  buf = std::move(new_buf);
}

folly::fbstring Output::Buf::moveToFbString() {
  // fbstring requires the data to be NUL-terminated. The terminator isn't
  // included in the size.
  *buffer(1) = 0;
  const auto cap = capacity();
  const auto size = len;
  len = 0; // so that the Output remains valid after buf.release()
  return folly::fbstring(
   reinterpret_cast<char *>(buf.release()),
   size,
   cap,
   folly::AcquireMallocatedString());
}

}
}
}
