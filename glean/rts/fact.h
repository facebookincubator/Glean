// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/rts/binary.h"
#include "glean/rts/id.h"

#include <boost/intrusive/list.hpp>

namespace facebook {
namespace glean {
namespace rts {

/**
 * A Glean fact
 *
 * Facts use a compact memory layout: a fact occupies one memory block where
 * statically sized data (class Fact) is followed by the key and the value:
 *
 * +------+-----+-------+
 * | Fact | key | value |
 * +------+-----+-------+
 *
 */
class Fact {
public:
  static constexpr uint32_t MAX_KEY_SIZE = 0xFFFFFFF;

  struct Clause {
    const unsigned char *data = nullptr;
    uint32_t key_size = 0;
    uint32_t value_size = 0;

    folly::ByteRange key() const {
      return {data, key_size};
    }

    folly::ByteRange value() const {
      return {data+key_size, value_size};
    }

    size_t size() const {
      return key_size + value_size;
    }

    folly::ByteRange bytes() const {
      return {data, size()};
    }

    static Clause from(folly::ByteRange bytes, size_t key_size) {
      assert(key_size <= MAX_KEY_SIZE);
      const size_t value_size = bytes.size() - key_size;
      assert(value_size <= 0xFFFFFFFF);
      return {
        bytes.data(),
        static_cast<uint32_t>(key_size),
        static_cast<uint32_t>(value_size)
      };
    }

    static Clause fromKey(folly::ByteRange bytes) {
      const size_t key_size = bytes.size();
      assert(key_size <= MAX_KEY_SIZE);
      return {bytes.data(), static_cast<uint32_t>(key_size), 0};
    }
  };

  struct Ref {
    Id id = Id::invalid();
    Pid type = Pid::invalid();
    Clause clause;

    static Ref invalid() {
      return {};
    }

    explicit operator bool() const {
      return bool(id);
    }

    folly::ByteRange key() const {
      return clause.key();
    }

    folly::ByteRange value() const {
      return clause.value();
    }
  };

  Id id() const {
    return id_;
  }

  Pid type() const {
    return type_;
  }

  Clause clause() const {
    return
      {reinterpret_cast<const unsigned char *>(this+1), key_size, value_size};
  }

  Ref ref() const {
    return Ref{id(), type(), clause()};
  }

  folly::ByteRange key() const {
    return clause().key();
  }

  folly::ByteRange value() const {
    return clause().value();
  }

  unsigned int tag() const {
    return tag_;
  }

  bool operator==(const Fact& other) const {
    return id_ == other.id_
            && type_ == other.type_
            && key_size == other.key_size
            && value_size == other.value_size
            && key() == other.key()
            && value() == other.value()
            && tag() == other.tag();
  }

  bool operator!=(const Fact& other) const {
    return !(*this == other);
  }

  static size_t size(size_t key_size, size_t value_size) {
    return sizeof(Fact) + key_size + value_size;
  }

  size_t size() const {
    return size(key_size, value_size);
  }

  static void serialize(binary::Output& output, Pid type, Clause clause);

  void serialize(binary::Output& output) const {
    serialize(output, type(), clause());
  }

  static void deserialize(binary::Input& input, Pid& type, Clause& clause);

  std::string dump() const;

  static void destroy(const Fact *fact) {
    if (fact) {
      fact->Fact::~Fact();
      ::operator delete(const_cast<Fact *>(fact));
    }
  }

  struct deleter {
    void operator()(const Fact *fact) const {
      Fact::destroy(fact);
    }
  };

  using unique_ptr = std::unique_ptr<Fact, deleter>;

  static unique_ptr create(Ref ref, unsigned int tag = 0) {
    // this should be checked earlier hence just an assert here
    assert(ref.clause.key_size <= 0xFFFFFFF);
    const auto size = ref.clause.size();
    Fact *fact = static_cast<Fact *>(::operator new(sizeof(Fact) + size));
    new(fact) Fact();
    fact->id_ = ref.id;
    fact->type_ = ref.type;
    fact->key_size = ref.clause.key_size;
    fact->tag_ = tag;
    fact->value_size = ref.clause.value_size;
    // memcpy(p, 0, 0) is undefined behaviour according to the C standard,
    // apparently, and this gets flagged in debug mode
    if (size > 0) {
      memcpy(
        reinterpret_cast<unsigned char *>(fact+1),
        ref.clause.data,
        size);
    }
    return unique_ptr(fact);
  }

private:
  Fact() {}
  ~Fact() {}

  Fact(const Fact&) = delete;
  Fact(Fact&&) = delete;
  Fact& operator=(const Fact&) = delete;
  Fact& operator=(Fact&&) = delete;

  boost::intrusive::list_member_hook<> list_hook;
  Id id_;
  Pid type_;
  struct {
    unsigned int tag_ : 4;
    unsigned int key_size : 28;
  };
  uint32_t value_size;

public:
  using intrusive_list = boost::intrusive::list<Fact,
    boost::intrusive::member_hook<
    Fact,
    boost::intrusive::list_member_hook<>,
    &Fact::list_hook>>;
};

}
}
}
