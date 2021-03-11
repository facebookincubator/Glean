#include "glean/ffi/wrap.h"
#include "glean/interprocess/cpp/counters.h"
#include "glean/interprocess/cpp/counters_ffi.h"

#include <fstream>
#include <unordered_map>

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

using namespace facebook::glean;

extern "C" {

struct glean_interprocess_counters_t final
    : public facebook::glean::interprocess::Counters{
  boost::interprocess::file_mapping mapping;
  boost::interprocess::mapped_region region;
  std::atomic<uint64_t> *counters;
  size_t size;

  explicit glean_interprocess_counters_t(const char *path, size_t n) {
    mapping = boost::interprocess::file_mapping(
      path, boost::interprocess::read_write);
    region = boost::interprocess::mapped_region(
      mapping,
      boost::interprocess::read_write,
      0,
      n * sizeof(uint64_t));
    counters = static_cast<std::atomic<uint64_t> *>(region.get_address());
    size = n;
    assert(std::atomic_is_lock_free(counters));
  }

  static void create(
      const char *path,
      size_t n) {
    std::vector<uint64_t> contents(n, 0);
    std::ofstream stream(path, std::ios::out | std::ios::binary);
    stream.write(
      reinterpret_cast<const char *>(contents.data()), n * sizeof(uint64_t));
  }

  std::atomic<uint64_t> *counter(size_t i) override {
    if (i >= size) {
      throw std::invalid_argument("counter index out of range");
    }
    return counters + i;
  }
};

const char *glean_interprocess_counters_create(const char *path, size_t size) {
  return ffi::wrap([=] {
    glean_interprocess_counters_t::create(path, size);
  });
}

const char *glean_interprocess_counters_open(
    const char *path,
    size_t size,
    glean_interprocess_counters_t **counters) {
  return ffi::wrap([=] {
    *counters = new glean_interprocess_counters_t(path, size);
  });
}

void glean_interprocess_counters_close(
    glean_interprocess_counters_t *counters) {
  ffi::free_(counters);
}

const char *glean_interprocess_counters_set(
    glean_interprocess_counters_t *counters,
    size_t index,
    uint64_t value) {
  return ffi::wrap([=] {
    counters->counter(index)->store(value);
  });
}

const char *glean_interprocess_counters_get(
    glean_interprocess_counters_t *counters,
    size_t index,
    uint64_t *value) {
  return ffi::wrap([=] {
    *value = counters->counter(index)->load();
  });
}

}

namespace facebook {
namespace glean {
namespace interprocess {

void countersSetup(const std::string& path, size_t size) {
  glean_interprocess_counters_t::create(path.c_str(), size);
}

std::unique_ptr<Counters> counters(const std::string& path, size_t size) {
  return std::make_unique<glean_interprocess_counters_t>(path.c_str(), size);
}

}
}
}
