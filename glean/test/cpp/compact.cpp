#include "glean/ffi/memory.h"
#include "glean/ffi/wrap.h"
#include "glean/schema/gen-cpp2/glean_test_types.h"

#include <folly/Range.h>
#include <thrift/lib/cpp2/protocol/Serializer.h>

using namespace facebook::glean;

extern "C" {

const char *glean_test_compact_reencode(
    const uint8_t *data,
    size_t size,
    uint8_t **out_data,
    size_t *out_size) {
  return ffi::wrap([=] {
    size_t consumed;
    auto fact = apache::thrift::CompactSerializer::deserialize<schema::glean::test::Predicate>(
      folly::ByteRange(data, size),
      &consumed);
    if (consumed != size) {
      throw std::logic_error("extra bytes");
    }
    ffi::clone_bytes(
      apache::thrift::CompactSerializer::serialize<std::string>(fact))
      .release_to(out_data, out_size);
  });
}

}
