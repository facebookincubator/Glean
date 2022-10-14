/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <utility>

#include <folly/Range.h>
#include <folly/container/F14Map.h>

#include <rocksdb/db.h>
#include <rocksdb/filter_policy.h>
#include <rocksdb/slice_transform.h>
#include <rocksdb/table.h>
#include <rocksdb/utilities/backup_engine.h>

#include "glean/rocksdb/rocksdb.h"
#include "glean/rocksdb/stats.h"
#ifdef FACEBOOK
#include "glean/facebook/rocksdb/rocksdb.h"
#endif
#include "glean/rts/binary.h"
#include "glean/rts/factset.h"
#include "glean/rts/nat.h"
#include "glean/rts/ownership/setu32.h"
#include "glean/rts/timer.h"

namespace facebook {
namespace glean {
namespace rocks {

using namespace rts;

namespace {

[[noreturn]] void error(const rocksdb::Status& s) {
  rts::error("rocksdb: " + s.ToString());
}

void check(const rocksdb::Status& status) {
  if (!status.ok()) {
    error(status);
  }
}

folly::ByteRange byteRange(const rocksdb::Slice& slice) {
  return folly::ByteRange(
    reinterpret_cast<const unsigned char *>(slice.data()), slice.size());
}

rocksdb::Slice slice(const folly::ByteRange& range) {
  return rocksdb::Slice(
    reinterpret_cast<const char *>(range.data()), range.size());
}

rocksdb::Slice slice(binary::Output& output) {
  return slice(output.bytes());
}

template<typename T>
rocksdb::Slice toSlice(const T& x) {
  return rocksdb::Slice(
    reinterpret_cast<const char *>(&x), sizeof(x));
}

template<typename T>
T fromSlice(const rocksdb::Slice& slice) {
  assert(slice.size() == sizeof(T));
  T x;
  std::memcpy(&x, slice.data(), slice.size());
  return x;
}

binary::Input input(const rocksdb::Slice& slice) {
  return binary::Input(byteRange(slice));
}

}


namespace {

struct Family {
private:
  template<typename F>
  Family(const char *n, F&& o, bool keep_ = true)
    : index(families.size()), name(n), options(std::forward<F>(o)), keep(keep_)
  {
    families.push_back(this);
  }

  Family(const Family&) = delete;
  Family& operator=(const Family&) = delete;

  static std::vector<const Family *> families;

public:
  size_t index;
  const char *name;
  std::function<void(rocksdb::ColumnFamilyOptions&)> options;

  // Whether to keep this column family after the DB is complete. If
  // keep = false, then the contents of the column family will be
  // deleted before compaction.
  bool keep = true;

  static const Family admin;
  static const Family entities;
  static const Family keys;
  static const Family stats;
  static const Family meta;
  static const Family ownershipUnits;
  static const Family ownershipUnitIds;
  static const Family ownershipRaw;
  static const Family ownershipDerivedRaw;
  static const Family ownershipSets;
  static const Family factOwners;

  static size_t count() { return families.size(); }

  static const Family * FOLLY_NULLABLE family(const std::string& name) {
    for (auto p : families) {
      if (name == p->name) {
        return p;
      }
    }
    return nullptr;
  }

  static const Family * FOLLY_NULLABLE family(size_t i) {
    return i < families.size() ? families[i] : nullptr;
  }
};

std::vector<const Family *> Family::families;

const Family Family::admin("admin", [](auto& opts){
  opts.OptimizeForPointLookup(100); });
const Family Family::entities("entities", [](auto& opts){
  // NOTE: Setting inplace_update_support=true leads to rocksdb assertion
  // failures when iteration backwards.
  opts.inplace_update_support = false;
  opts.OptimizeForPointLookup(100); });
const Family Family::keys("keys", [](auto& opts) {
  opts.prefix_extractor.reset(
    rocksdb::NewFixedPrefixTransform(sizeof(Id::word_type))); });
const Family Family::stats("stats", [](auto& opts) {
  opts.OptimizeForPointLookup(10); });
const Family Family::meta("meta", [](auto&) {});
const Family Family::ownershipUnits("ownershipUnits", [](auto& opts) {
  opts.OptimizeForPointLookup(100); });
const Family Family::ownershipUnitIds("ownershipUnitIds", [](auto& opts) {
  opts.OptimizeForPointLookup(100); });
const Family Family::ownershipRaw("ownershipRaw", [](auto&) {}, false);
const Family Family::ownershipDerivedRaw("ownershipDerivedRaw", [](auto& opts) {
  opts.inplace_update_support = false; }, false);
const Family Family::ownershipSets("ownershipSets", [](auto& opts){
  opts.inplace_update_support = false; });
const Family Family::factOwners("factOwners", [](auto& opts){
  opts.inplace_update_support = false; });

enum class AdminId : uint32_t {
  NEXT_ID,
  VERSION,
  STARTING_ID
};

const char *admin_names[] = {
  "NEXT_ID",
  "VERSION",
  "STARTING_ID"
};

#ifndef FACEBOOK
namespace {
rocksdb::Status openRocksDB(
    const rocksdb::Options& options,
    const std::string& name,
    rocksdb::DB** dbptr,
    const std::vector<rocksdb::ColumnFamilyDescriptor>& column_families,
    std::vector<rocksdb::ColumnFamilyHandle*>* handles,
    bool read_only) {
  if (read_only) {
    return rocksdb::DB::OpenForReadOnly(
        options, name, column_families, handles, dbptr);
  } else {
    return rocksdb::DB::Open(options, name, column_families, handles, dbptr);
  }
}
} // namespace
#endif

struct ContainerImpl final : Container {
  Mode mode;
  rocksdb::Options options;
  rocksdb::WriteOptions writeOptions;
  std::unique_ptr<rocksdb::DB> db;
  std::vector<rocksdb::ColumnFamilyHandle *> families;

  ContainerImpl(
      const std::string& path,
      Mode m,
      bool cache_index_and_filter_blocks,
      folly::Optional<std::shared_ptr<Cache>> cache) {
    mode = m;

    if (mode == Mode::Create) {
      options.error_if_exists = true;
      options.create_if_missing = true;
    } else {
      options.error_if_exists = false;
      options.create_if_missing = false;
    }

    options.inplace_update_support = true;
    options.allow_concurrent_memtable_write = false;

    {
      rocksdb::BlockBasedTableOptions table_options;
      if (cache) {
        table_options.block_cache = std::move(cache.value());
      }
      table_options.filter_policy.reset(
        rocksdb::NewBloomFilterPolicy(10, false));
      table_options.whole_key_filtering = true;

      if (cache_index_and_filter_blocks) {
        table_options.cache_index_and_filter_blocks = true;
        table_options.cache_index_and_filter_blocks_with_high_priority = true;
        table_options.pin_l0_filter_and_index_blocks_in_cache = true;
      }

      options.table_factory.reset(
        rocksdb::NewBlockBasedTableFactory(table_options));
    }

    // options.IncreaseParallelism();
    // options.compression = rocksdb::CompressionType::kNoCompression;
    // writeOptions.sync = false;
    // writeOptions.disableWAL = true;

    families.resize(Family::count(), nullptr);
    std::vector<std::string> names;
    if (mode != Mode::Create) {
      check(rocksdb::DB::ListColumnFamilies(options,path,&names));
    }

    std::vector<rocksdb::ColumnFamilyDescriptor> existing;
    std::vector<rocksdb::ColumnFamilyHandle**> ptrs;
    for (const auto& name : names) {
      if (name != rocksdb::kDefaultColumnFamilyName) {
        if (auto family = Family::family(name)) {
          rocksdb::ColumnFamilyOptions opts(options);
          family->options(opts);
          existing.push_back(rocksdb::ColumnFamilyDescriptor(name, opts));
          ptrs.push_back(&families[family->index]);
        } else {
          rts::error("Unknown column family '{}'", name);
        }
      }
    }
    existing.push_back(
      rocksdb::ColumnFamilyDescriptor(
        rocksdb::kDefaultColumnFamilyName, options));
    ptrs.push_back(nullptr);

    std::vector<rocksdb::ColumnFamilyHandle*> hs;
    rocksdb::DB *db_ptr;
    check(openRocksDB(
                options,
                path,
                &db_ptr,
                existing,
                &hs,
                mode == Mode::ReadOnly));
    if (!db_ptr) {
      rts::error("got nullptr from rocksdb");
    } else {
      db.reset(db_ptr);
    }

    assert(hs.size() == ptrs.size());
    for (size_t i = 0; i < ptrs.size(); ++i) {
      if (ptrs[i] != nullptr) {
        *ptrs[i] = hs[i];
      } else {
        db->DestroyColumnFamilyHandle(hs[i]);
      }
    }

    for (size_t i = 0; i < families.size(); ++i) {
      if (families[i] == nullptr) {
        auto family = Family::family(i);
        assert(family != nullptr);

        rocksdb::ColumnFamilyOptions opts(options);
        family->options(opts);
        check(db->CreateColumnFamily(
          opts,
          family->name,
          &families[i]));
      }
    }
  }

  ContainerImpl(const ContainerImpl&) = delete;
  ContainerImpl(ContainerImpl&& other) = default;
  ContainerImpl& operator=(const ContainerImpl&) = delete;
  ContainerImpl& operator=(ContainerImpl&&) = delete;

  ~ContainerImpl() override {
    close();
  }

  void close() noexcept override {
    if (db) {
      for (auto handle : families) {
        if (handle) {
          try {
            db->DestroyColumnFamilyHandle(handle);
          } catch(const std::exception& e) {
            LOG(ERROR) << e.what();
          } catch(...) {
            LOG(ERROR) << "unknown error while closing column family";
          }
        }
      }
      families.resize(0);
      db.reset();
    }
  }

  void requireOpen() const {
    if (!db) {
      rts::error("rocksdb: database is closed");
    }
  }

  rocksdb::ColumnFamilyHandle *family(const Family& family) const {
    assert(family.index < families.size());
    return families[family.index];
  }


  void writeData(folly::ByteRange key, folly::ByteRange value) override {
    requireOpen();
    check(db->Put(
      writeOptions,
      family(Family::meta),
      slice(key),
      slice(value)));
  }

  bool readData(
      folly::ByteRange key,
      std::function<void(folly::ByteRange)> f) override {
    requireOpen();
    rocksdb::PinnableSlice val;
    auto s = db->Get(
      rocksdb::ReadOptions(),
      family(Family::meta),
      slice(key),
      &val);
    if (s.IsNotFound()) {
      return false;
    } else {
      check(s);
      f(byteRange(val));
      return true;
    }
  }

  void optimize() override {
    for (uint32_t i = 0; i < families.size(); i++) {
      auto family = Family::family(i);
      auto handle = families[i];
      if (handle && family) {
        if (!family->keep) {
          // delete the contents of this column family
          check(db->DropColumnFamily(handle));
          db->DestroyColumnFamilyHandle(handle);
          rocksdb::ColumnFamilyOptions opts(options);
          family->options(opts);
          check(db->CreateColumnFamily(
            opts,
            family->name,
            &handle));
          families[i] = handle;
        }
        const auto nlevels = db->NumberLevels(handle);
        if (nlevels != 2) {
          rocksdb::CompactRangeOptions copts;
          check(db->CompactRange(copts, handle, nullptr, nullptr));
        }
      }
    }
  }

  static std::unique_ptr<rocksdb::BackupEngine> backupEngine(
      const std::string& path) {
    rocksdb::BackupEngine *p;
    check(rocksdb::BackupEngine::Open(
      rocksdb::Env::Default(),
      rocksdb::BackupEngineOptions(path),
      &p));
    return std::unique_ptr<rocksdb::BackupEngine>(p);
  }

  void backup(const std::string& path) override {
    requireOpen();
    bool flush { mode != Mode::ReadOnly };
    check(backupEngine(path)->CreateNewBackup(db.get(), flush));
  }

  std::unique_ptr<Database> openDatabase(Id start, int32_t version) && override;
};

void serializeEliasFano(binary::Output& out, const OwnerSet& set) {
  out.nat(set.size);
  out.nat(set.numLowerBits);
  out.nat(set.upperSizeBytes);
  out.nat(set.skipPointers - set.data.begin());
  out.nat(set.forwardPointers - set.data.begin());
  out.nat(set.lower - set.data.begin());
  out.nat(set.upper - set.data.begin());
  out.put(set.data);
}

void deserializeEliasFano(binary::Input& in, OwnerSet& set) {
  set.size = in.trustedNat();
  set.numLowerBits = in.trustedNat();
  set.upperSizeBytes = in.trustedNat();
  auto skipPointers = in.trustedNat();
  auto forwardPointers = in.trustedNat();
  auto lower = in.trustedNat();
  auto upper = in.trustedNat();
  set.data = in.bytes();
  set.skipPointers = set.data.begin() + skipPointers;
  set.forwardPointers = set.data.begin() + forwardPointers;
  set.lower = set.data.begin() + lower;
  set.upper = set.data.begin() + upper;
}

struct DatabaseImpl final : Database {
  int64_t db_version;
  ContainerImpl container_;
  Id starting_id;
  Id next_id;
  AtomicPredicateStats stats_;
  std::vector<size_t> ownership_unit_counters;
  folly::F14FastMap<uint64_t,size_t> ownership_derived_counters;

  // Cached ownership sets, only used when writing.
  // TODO: initialize this lazily
  std::unique_ptr<Usets> usets_;

  explicit DatabaseImpl(ContainerImpl c, Id start, int64_t version)
      : container_(std::move(c)) {
    starting_id = Id::fromWord(getAdminValue(
      AdminId::STARTING_ID,
      start.toWord(),
      container_.mode == Mode::Create,
      []{}));

    next_id = Id::fromWord(getAdminValue(
      AdminId::NEXT_ID,
      start.toWord(),
      container_.mode == Mode::Create,
      [mode = container_.mode] {
        if (mode != Mode::Create) {
          rts::error("corrupt database - missing NEXT_ID");
        }
      }));

    db_version = getAdminValue(
      AdminId::VERSION,
      version,
      container_.mode == Mode::Create,
      []{});

    if (db_version != version) {
      rts::error("unexpected database version {}", db_version);
    }

    stats_.set(loadStats());
    ownership_unit_counters = loadOwnershipUnitCounters();
    ownership_derived_counters = loadOwnershipDerivedCounters();

    // We only need usets_ for writable DBs, and it takes time and
    // memory to load them so omit this for ReadOnly DBs.
    if (container_.mode != Mode::ReadOnly) {
      usets_ = loadOwnershipSets();
    }

    // Enable the fact owner cache when the DB is read-only
    if (container_.mode == Mode::ReadOnly) {
      factOwnerCache_.enable();
    }
  }

  DatabaseImpl(const DatabaseImpl&) = delete;
  DatabaseImpl& operator=(const DatabaseImpl&) = delete;
  DatabaseImpl(DatabaseImpl&&) = delete;
  DatabaseImpl& operator=(DatabaseImpl&&) = delete;

  Container& container() noexcept override {
    return container_;
  }

  template<typename T, typename F>
  T getAdminValue(AdminId id, T def, bool write, F&& notFound) {
    container_.requireOpen();
    rocksdb::PinnableSlice val;
    binary::Output key;
    key.fixed(id);
    auto s = container_.db->Get(
      rocksdb::ReadOptions(),
      container_.family(Family::admin),
      slice(key),
      &val);
    if (!s.IsNotFound()) {
      check(s);
      binary::Input value = input(val);
      auto result = value.fixed<T>();
      if (!value.empty()) {
        rts::error("corrupt database - invalid {}",
          admin_names[static_cast<uint32_t>(id)]);
      }
      return result;
    } else {
      notFound();
      if (write) {
        binary::Output value;
        value.fixed(def);
        check(container_.db->Put(
          container_.writeOptions,
          container_.family(Family::admin),
          slice(key),
          slice(value)));
      }
      return def;
    }
  }

  rts::PredicateStats loadStats() {
    container_.requireOpen();
    rts::PredicateStats stats;
    std::unique_ptr<rocksdb::Iterator> iter(
      container_.db->NewIterator(
        rocksdb::ReadOptions(),
        container_.family(Family::stats)
      )
    );
    if (!iter) {
      rts::error("rocksdb: couldn't allocate iterator");
    }

    for (iter->SeekToFirst(); iter->Valid(); iter->Next()) {
      binary::Input key(byteRange(iter->key()));
      stats[key.fixed<Pid>()] = fromSlice<MemoryStats>(iter->value());
      assert(key.empty());
    }
    auto s = iter->status();
    if (!s.IsNotFound()) {
      check(s);
    }
    return stats;
  }

  std::vector<size_t> loadOwnershipUnitCounters() {
    container_.requireOpen();
    std::vector<size_t> result;

    std::unique_ptr<rocksdb::Iterator> iter(
      container_.db->NewIterator(
        rocksdb::ReadOptions(),
        container_.family(Family::ownershipRaw)));

    if (!iter) {
      rts::error("rocksdb: couldn't allocate iterator");
    }

    for (iter->SeekToFirst(); iter->Valid(); iter->Next()) {
      binary::Input key(byteRange(iter->key()));
      auto id = key.trustedNat();
      if (id == result.size()) {
        result.push_back(0);
      } else if (id+1 == result.size()) {
        ++result.back();
      } else {
        rts::error("rocksdb: invalid ownershipUnits");
      }
    }

    return result;
  }

  folly::F14FastMap<uint64_t,size_t> loadOwnershipDerivedCounters() {
    container_.requireOpen();
    folly::F14FastMap<uint64_t,size_t> result;

    std::unique_ptr<rocksdb::Iterator> iter(
      container_.db->NewIterator(
        rocksdb::ReadOptions(),
        container_.family(Family::ownershipDerivedRaw)));

    if (!iter) {
      rts::error("rocksdb: couldn't allocate iterator");
    }

    for (iter->SeekToFirst(); iter->Valid(); iter->Next()) {
      binary::Input key(byteRange(iter->key()));
      auto pid = key.trustedNat();
      const auto [i,_] = result.insert({pid,0});
      ++i->second;
    }

    VLOG(1) << "derived fact owners for " << result.size() << " pids";
    return result;
  }

  std::unique_ptr<Usets> loadOwnershipSets() {
    auto t = makeAutoTimer("loadOwnershipSets");

    auto iter = this->getSetIterator();
    auto [first,size] = iter->sizes();

    auto usets = std::make_unique<Usets>(first + size);

    while (const auto pair = iter->get()) {
      auto set = SetU32::fromEliasFano(*pair->second.set);
      auto p = usets->add(std::move(set),0);
      p->id = pair->first;
    }
    auto stats = usets->statistics();

    LOG(INFO) << "loadOwnershipSets loaded " << stats.adds << " sets, " <<
      stats.bytes << " bytes";

    return usets;
  }

  folly::Optional<uint32_t> getUnitId(folly::ByteRange unit) override {
    rocksdb::PinnableSlice val;
    auto s = container_.db->Get(
        rocksdb::ReadOptions(),
        container_.family(Family::ownershipUnits),
        slice(unit),
        &val);
    if (!s.IsNotFound()) {
      check(s);
      assert(val.size() == sizeof(uint32_t));
      return folly::loadUnaligned<uint32_t>(val.data());
    } else {
      return folly::none;
    }
  }

  folly::Optional<std::string> getUnit(uint32_t unit_id) override {
    rocksdb::PinnableSlice val;
    auto s = container_.db->Get(
        rocksdb::ReadOptions(),
        container_.family(Family::ownershipUnitIds),
        slice(EncodedNat(unit_id).byteRange()),
        &val);
    if (!s.IsNotFound()) {
      check(s);
      return val.ToString();
    } else {
      return folly::none;
    }
  }

  rts::Id startingId() const override {
    return starting_id;
  }

  rts::Id firstFreeId() const override {
    return next_id;
  }

  Id idByKey(Pid type, folly::ByteRange key) override {
    if (count(type).high() == 0) {
      return Id::invalid();
    }

    container_.requireOpen();
    rocksdb::PinnableSlice out;
    binary::Output k;
    k.fixed(type);
    k.put(key);
    auto s = container_.db->Get(
      rocksdb::ReadOptions(),
      container_.family(Family::keys),
      slice(k),
      &out);
    if (s.IsNotFound()) {
      return Id::invalid();
    } else {
      check(s);
      binary::Input value = input(out);
      auto id = value.fixed<Id>();
      assert(value.empty());
      return id;
    }
  }

  bool lookupById(Id id, rocksdb::PinnableSlice &val) const {
    if (id < startingId() || id >= firstFreeId()) {
      return false;
    }
    binary::Output key;
    key.nat(id.toWord());
    val.Reset();
    auto s = container_.db->Get(
      rocksdb::ReadOptions(),
      container_.family(Family::entities),
      slice(key),
      &val
    );
    if (s.IsNotFound()) {
      return false;
    } else {
      check(s);
      return true;
    }
  }

  static rts::Fact::Ref decomposeFact(Id id, const rocksdb::Slice& data) {
    auto inp = input(data);
    const auto ty = inp.packed<Pid>();
    const auto key_size = inp.packed<uint32_t>();
    return
      rts::Fact::Ref{id, ty, rts::Fact::Clause::from(inp.bytes(), key_size)};
  }

  Pid typeById(Id id) override {
    container_.requireOpen();
    rocksdb::PinnableSlice val;
    if (lookupById(id, val)) {
      return input(val).packed<Pid>();
    } else {
      return Pid::invalid();
    }
  }

  bool factById(Id id, std::function<void(Pid, Fact::Clause)> f) override {
    container_.requireOpen();
    rocksdb::PinnableSlice val;
    if (lookupById(id, val)) {
      auto ref = decomposeFact(id, val);
      f(ref.type, ref.clause);
      return true;
    } else {
      return false;
    }
  }

  struct SeekIterator final : rts::FactIterator {
    SeekIterator(
        folly::ByteRange start,
        size_t prefix_size,
        Pid type,
        const DatabaseImpl *db)
      : upper_bound_(binary::lexicographicallyNext({start.data(), prefix_size}))
      , upper_bound_slice_(
          reinterpret_cast<const char *>(upper_bound_.data()),
          upper_bound_.size())
      , type_(type)
      , db_(db)
    {
      assert(prefix_size <= start.size());
      // both upper_bound_slice_ and options_ need to be alive for the duration
      // of the iteration
      options_.iterate_upper_bound = &upper_bound_slice_;
      iter_.reset(
        db->container_.db->NewIterator(
          options_,
          db->container_.family(Family::keys)));
      if (iter_) {
        iter_->Seek(slice(start));
      } else {
        rts::error("rocksdb: couldn't allocate iterator");
      }
    }

    void next() override {
      iter_->Next();
      auto s = iter_->status();
      if (!s.IsNotFound()) {
        check(s);
      }
    }

    Fact::Ref get(Demand demand) override {
      if (iter_->Valid()) {
        auto key = input(iter_->key());
        auto ty = key.fixed<Pid>();
        assert(ty == type_);
        auto value = input(iter_->value());
        auto id = value.fixed<Id>();
        assert(value.empty());

        if (demand == KeyOnly) {
          return Fact::Ref{id, type_, Fact::Clause::fromKey(key.bytes())};
        } else {
          auto found = db_->lookupById(id, slice_);
          assert(found);
          return decomposeFact(id, slice_);
        }
      } else {
        return Fact::Ref::invalid();
      }
    }

    std::optional<Id> lower_bound() override { return std::nullopt; }
    std::optional<Id> upper_bound() override { return std::nullopt; }

    const std::vector<unsigned char> upper_bound_;
    const rocksdb::Slice upper_bound_slice_;
    const Pid type_;
    rocksdb::ReadOptions options_;
    std::unique_ptr<rocksdb::Iterator> iter_;
    const DatabaseImpl *db_;
    rocksdb::PinnableSlice slice_;
  };

  std::unique_ptr<rts::FactIterator> seek(
      Pid type, folly::ByteRange start, size_t prefix_size) override {
    assert(prefix_size <= start.size());
    if (count(type).high() == 0) {
      return std::make_unique<EmptyIterator>();
    }

    container_.requireOpen();
    binary::Output out;
    out.fixed(type);
    const auto type_size = out.size();
    out.put(start);
    return std::make_unique<SeekIterator>(
      out.bytes(),
      type_size + prefix_size,
      type,
      this);
  }

  std::unique_ptr<rts::FactIterator> seekWithinSection(
      Pid type,
      folly::ByteRange start,
      size_t prefix_size,
      Id from,
      Id upto) override {

    if (upto <= startingId() || firstFreeId() <= from) {
      return std::make_unique<EmptyIterator>();
    }

    return Section(this, from, upto).seek(type, start, prefix_size);
  }

  // Legacy DBs don't store fact ids as 8 byte little-endian numbers so we can't
  // seek over those in lexicographic order. Instead, just look up each fact id
  // in the DB (much slower).
  template<typename Direction>
  struct LegacyEnumerateIterator final : rts::FactIterator {
    LegacyEnumerateIterator(Id from, Id stop, const DatabaseImpl *db)
        : stop_(stop)
        , db_(db) {
      find(from);
    }

    void next() override {
      find(Direction::advance(id_));
    }

    Fact::Ref get(Demand) override {
      return Direction::more(id_, stop_)
        ? decomposeFact(id_, slice_)
        : Fact::Ref::invalid();
    }

    void find(Id from) {
      for (id_ = from;
          Direction::more(id_, stop_) && !db_->lookupById(id_, slice_);
          id_ = Direction::advance(id_))
      {}
    }

    std::optional<Id> lower_bound() override { return std::nullopt; }
    std::optional<Id> upper_bound() override { return std::nullopt; }

    Id id_;
    Id stop_;
    const DatabaseImpl *db_;
    rocksdb::PinnableSlice slice_;
  };

  template<typename Direction>
  struct EnumerateIterator final : rts::FactIterator {
    static std::vector<char> encode(Id id) {
      std::vector<char> v(rts::MAX_NAT_SIZE);
      const auto n = rts::storeNat(
        reinterpret_cast<unsigned char *>(v.data()),
        id.toWord());
      v.resize(n);
      return v;
    }

    explicit EnumerateIterator(Id start, Id bound, const DatabaseImpl *db)
      : bound_(encode(bound))
      , bound_slice_(bound_.data(), bound_.size())
    {
      // both the slice and options_ need to be alive for the duration
      // of the iteration
      options_.*Direction::iterate_bound = &bound_slice_;
      iter_.reset(
        db->container_.db->NewIterator(
          options_,
          db->container_.family(Family::entities)));

      auto st = encode(start);
      if (iter_) {
        (iter_.get()->*Direction::seek)({st.data(), st.size()});
      } else {
        rts::error("rocksdb: couldn't allocate iterator");
      }
    }

    void next() override {
      (iter_.get()->*Direction::next)();
      auto s = iter_->status();
      if (!s.IsNotFound()) {
        check(s);
      }
    }

    Fact::Ref get(Demand demand) override {
      return iter_->Valid()
        ? decomposeFact(
            Id::fromWord(
              loadTrustedNat(
                reinterpret_cast<const unsigned char *>(
                  iter_->key().data())).first),
                  iter_->value())
        : Fact::Ref::invalid();
    }

    std::optional<Id> lower_bound() override { return std::nullopt; }
    std::optional<Id> upper_bound() override { return std::nullopt; }

    const std::vector<char> bound_;
    const rocksdb::Slice bound_slice_;
    rocksdb::ReadOptions options_;
    std::unique_ptr<rocksdb::Iterator> iter_;
  };

  struct Forward {
    static std::pair<Id,Id> bounds(
        Id from, Id upto, Id starting_id, Id next_id) {
      if (from >= next_id || (upto && upto <= starting_id)) {
        return {Id::invalid(), Id::invalid()};
      } else {
        return {
          std::max(from, starting_id),
          upto && upto <= next_id ? upto : next_id
        };
      }
    }

    static inline constexpr auto iterate_bound =
      &rocksdb::ReadOptions::iterate_upper_bound;
    static inline constexpr auto seek = &rocksdb::Iterator::Seek;
    static inline constexpr auto next = &rocksdb::Iterator::Next;

    // legacy iterator support
    static Id advance(Id id) { return id + 1; }
    static bool more(Id current, Id stop) { return current < stop; }
  };

  struct Backward {
    static std::pair<Id,Id> bounds(
        Id from, Id downto, Id starting_id, Id next_id) {
      if (downto >= next_id || (from && from <= starting_id)) {
        return {Id::invalid(), Id::invalid()};
      } else {
        return {
          (from && from <= next_id ? from : next_id) - 1,
          std::max(downto, starting_id)
        };
      }
    }

    static inline constexpr auto iterate_bound =
      &rocksdb::ReadOptions::iterate_lower_bound;
    static inline constexpr auto seek = &rocksdb::Iterator::SeekForPrev;
    static inline constexpr auto next = &rocksdb::Iterator::Prev;

    // legacy iterator support
    static Id advance(Id id) { return id - 1; }
    static bool more(Id current, Id stop) { return current >= stop; }
  };

  template<typename Direction>
  std::unique_ptr<rts::FactIterator> makeEnumerateIterator(Id from, Id to) {
    container_.requireOpen();
    const auto [start, bound] = Direction::bounds(
      from, to, startingId(), firstFreeId());
    if (!start) {
      return std::make_unique<rts::EmptyIterator>();
    } else {
      return std::make_unique<EnumerateIterator<Direction>>(start, bound, this);
    }
  }

  std::unique_ptr<rts::FactIterator> enumerate(Id from, Id upto) override {
    return makeEnumerateIterator<Forward>(from, upto);
  }

  std::unique_ptr<rts::FactIterator> enumerateBack(
      Id from, Id downto) override {
    return makeEnumerateIterator<Backward>(from, downto);
  }

  rts::Interval count(Pid pid) const override {
    return stats_.count(pid);
  }

  rts::PredicateStats predicateStats() const override {
    return stats_.get();
  }

  void commit(rts::FactSet& facts) override {
    container_.requireOpen();

    if (facts.empty()) {
      return;
    }

    if (facts.startingId() < next_id) {
      rts::error("batch inserted out of sequence ({} < {})",
        facts.startingId(),
        next_id);
    }

    rocksdb::WriteBatch batch;

    // NOTE: We do *not* support concurrent writes so we don't need to protect
    // stats_ here because nothing should be able to replace it while we're
    // running
    const auto& old_stats = stats_.unprotected();
    PredicateStats new_stats(old_stats);

    for (auto iter = facts.enumerate(); auto fact = iter->get(); iter->next()) {
      assert(fact.id >= next_id);

      uint64_t mem = 0;
      auto put = [&](auto family, const auto& key, const auto& value) {
        check(batch.Put(
          family,
          key,
          value
        ));
        mem += key.size();
        mem += value.size();
      };

      {
        binary::Output k;
        k.nat(fact.id.toWord());
        binary::Output v;
        v.packed(fact.type);
        v.packed(fact.clause.key_size);
        v.put({fact.clause.data, fact.clause.size()});

        put(container_.family(Family::entities), slice(k), slice(v));
      }

      {
        binary::Output k;
        k.fixed(fact.type);
        k.put(fact.key());
        binary::Output v;
        v.fixed(fact.id);

        put(container_.family(Family::keys), slice(k), slice(v));
      }

      new_stats[fact.type] += MemoryStats::one(mem);
    }

    const auto first_free_id = facts.firstFreeId();
    check(batch.Put(
      container_.family(Family::admin),
      toSlice(AdminId::NEXT_ID),
      toSlice(first_free_id)));

    for (const auto& x : new_stats) {
      if (x.second != old_stats.get(x.first)) {
        check(batch.Put(
          container_.family(Family::stats),
          toSlice(x.first.toWord()),
          toSlice(x.second)));
      }
    }

    check(container_.db->Write(container_.writeOptions, &batch));
    next_id = first_free_id;

    stats_.set(std::move(new_stats));
  }

  void addOwnership(const std::vector<OwnershipSet>& ownership) override {
    container_.requireOpen();

    if (ownership.empty()) {
      return;
    }

    size_t new_count = 0;
    std::vector<size_t> touched;
    rocksdb::WriteBatch batch;

    for (const auto& set : ownership) {
      uint32_t unit_id;
      auto res = getUnitId(set.unit);
      if (res.hasValue()) {
        unit_id = *res;
        if (unit_id >= ownership_unit_counters.size()) {
          rts::error("inconsistent unit id {}", unit_id);
        }
        touched.push_back(unit_id);
      } else {
        unit_id = ownership_unit_counters.size() + new_count;
        check(batch.Put(
          container_.family(Family::ownershipUnits),
          slice(set.unit),
          toSlice(unit_id)));
        check(batch.Put(
          container_.family(Family::ownershipUnitIds),
          slice(EncodedNat(unit_id).byteRange()),
          slice(set.unit)));
        ++new_count;
      }

      binary::Output key;
      key.nat(unit_id);
      key.nat(unit_id < ownership_unit_counters.size()
        ? ownership_unit_counters[unit_id]
        : 0);
      check(batch.Put(
        container_.family(Family::ownershipRaw),
        slice(key),
        rocksdb::Slice(
          reinterpret_cast<const char *>(set.ids.data()),
          set.ids.size() * sizeof(int64_t))));
    }

    check(container_.db->Write(container_.writeOptions, &batch));

    for (auto i : touched) {
      assert(i < ownership_unit_counters.size());
      ++ownership_unit_counters[i];
    }
    ownership_unit_counters.insert(
      ownership_unit_counters.end(),  new_count, 1);
  }

  std::unique_ptr<rts::DerivedFactOwnershipIterator>
    getDerivedFactOwnershipIterator(Pid pid) override {
    struct DerivedFactIterator : rts::DerivedFactOwnershipIterator {
      explicit DerivedFactIterator(
          Pid pid, std::unique_ptr<rocksdb::Iterator> i)
        : pid_(pid), iter(std::move(i))
      {}

      folly::Optional<DerivedFactOwnership> get() override {
        if (iter->Valid()) {
          binary::Input key(byteRange(iter->key()));
          auto pid = key.trustedNat();
          if (pid != pid_.toWord()) {
            return {};
          }
          const auto val = iter->value();
          const size_t elts = val.size() /
            (sizeof(uint32_t) + sizeof(uint64_t));
          const Id* ids = reinterpret_cast<const Id *>(val.data());
          const UsetId* owners = reinterpret_cast<const UsetId *>(
              val.data() + elts * sizeof(uint64_t));
          iter->Next();
          return rts::DerivedFactOwnership{
            { ids, elts },
            { owners, elts }
          };
        } else {
          return folly::none;
        }
      }

      Pid pid_;
      std::unique_ptr<rocksdb::Iterator> iter;
    };

    std::unique_ptr<rocksdb::Iterator> iter(
      container_.db->NewIterator(
        rocksdb::ReadOptions(),
        container_.family(Family::ownershipDerivedRaw)));

    if (!iter) {
      rts::error("rocksdb: couldn't allocate derived ownership iterator");
    }

    EncodedNat key(pid.toWord());
    iter->Seek(slice(key.byteRange()));
    return std::make_unique<DerivedFactIterator>(pid, std::move(iter));
  }

  std::unique_ptr<rts::OwnershipUnitIterator>
      getOwnershipUnitIterator() override {
    struct UnitIterator : rts::OwnershipUnitIterator {
      explicit UnitIterator(std::unique_ptr<rocksdb::Iterator> i)
        : iter(std::move(i))
      {}

      folly::Optional<rts::OwnershipUnit> get() override {
        if (iter->Valid()) {
          binary::Input key(byteRange(iter->key()));
          auto unit = key.trustedNat();
          const auto val = iter->value();
          iter->Next();
          return rts::OwnershipUnit{
            static_cast<uint32_t>(unit),
            { reinterpret_cast<const OwnershipUnit::Ids *>(val.data()),
              val.size() / sizeof(OwnershipUnit::Ids) }
          };
        } else {
          return {};
        }
      }

      std::unique_ptr<rocksdb::Iterator> iter;
    };
    std::unique_ptr<rocksdb::Iterator> iter(
      container_.db->NewIterator(
        rocksdb::ReadOptions(),
        container_.family(Family::ownershipRaw)));

    if (!iter) {
      rts::error("rocksdb: couldn't allocate ownership unit iterator");
    }

    iter->SeekToFirst();
    return std::make_unique<UnitIterator>(std::move(iter));
  }

  void storeOwnership(ComputedOwnership &ownership) override;

  std::unique_ptr<rts::Ownership> getOwnership() override;

  std::unique_ptr<rts::OwnershipSetIterator> getSetIterator();

  void addDefineOwnership(DefineOwnership& define) override;

  void putOwnerSet(
      rocksdb::WriteBatch& batch,
      UsetId id,
      SetOp op,
      const OwnerSet& set) const {
    binary::Output key;
    key.nat(id);
    binary::Output value;
    value.nat(op);
    serializeEliasFano(value, set);
    check(batch.Put(container_.family(Family::ownershipSets), slice(key),
                    slice(value)));
  }

  // Cache for getOwner(Id). This is represented as a vector of pages
  // indexed by the upper 48 bits of the fact ID. Each page is a table
  // of intervals [(id1,set1), (id2,set2), ...], split into two arrays
  // because the IDs are 16 bits and we want that array to be as
  // compact as possible. We find the correct interval in getOwner()
  // by binary search in the page on the low 16 bits of the fact ID.
  // TODO: maybe it's worth optimizing very dense pages to be just a
  // lookup, and conversely very sparse pages to a linear search?

  struct FactOwnerCache {
    UsetId getOwner(ContainerImpl& container, Id id);
    void enable();

  private:
    struct Page {
      std::vector<uint16_t> factIds;
      std::vector<UsetId> setIds;
    };
    using Cache = std::vector<std::unique_ptr<Page>>;

    const Page* getPage(ContainerImpl& container, uint64_t prefix);

    // nullptr means the cache is disabled (while the DB is writable)
    folly::Synchronized<std::unique_ptr<Cache>> cache_;
    size_t size_;
  };

  FactOwnerCache factOwnerCache_;

  /// Enable the fact owner cache. This should only be called when the
  // DB is read-only.
  void cacheOwnership() override;
};

void DatabaseImpl::storeOwnership(ComputedOwnership &ownership) {
  container_.requireOpen();

  if (ownership.sets_.size() > 0) {
    auto t = makeAutoTimer("storeOwnership(sets)");
    rocksdb::WriteBatch batch;

    uint32_t id = ownership.firstId_;
    for (auto &exp : ownership.sets_) {
      if ((id % 1000000) == 0) {
        VLOG(1) << "storeOwnership: " << id;
      }
      putOwnerSet(batch, id, exp.op, exp.set);
      id++;
    }
    VLOG(1) << "storeOwnership: writing sets (" <<
      ownership.sets_.size() << ")";
    check(container_.db->Write(container_.writeOptions, &batch));
  }

  // ToDo: just update usets_, don't load the whole thing
  usets_ = loadOwnershipSets();

  if (ownership.facts_.size() > 0) {
    auto t = makeAutoTimer("storeOwnership(facts)");
    rocksdb::WriteBatch batch;
    for (uint64_t i = 0; i < ownership.facts_.size(); i++) {
      auto id = ownership.facts_[i].first;
      auto usetid = ownership.facts_[i].second;
      EncodedNat key(id.toWord());
      EncodedNat val(usetid);
      check(batch.Put(container_.family(Family::factOwners),
                      slice(key.byteRange()),
                      slice(val.byteRange())));
    }
    VLOG(1) << "storeOwnership: writing facts: " <<
      ownership.facts_.size() << " intervals";
    check(container_.db->Write(container_.writeOptions, &batch));
  }
}

struct StoredOwnership : Ownership {
  explicit StoredOwnership(DatabaseImpl *db) : db_(db) {}

  UsetId getOwner(Id id) override;

  UsetId nextSetId() override {
    return db_->usets_->getNextId();
  }

  UsetId lookupSet(Uset* uset) override {
    if (!db_->usets_) {
      rts::error("rocksdb: lookupSet on read-only DB");
    } else {
      auto existing = db_->usets_->lookup(uset);
      if (existing) {
        return existing->id;
      } else {
        return INVALID_USET;
      }
    }
  }

  folly::Optional<SetExpr<SetU32>> getUset(UsetId id) override {
    rocksdb::PinnableSlice val;
    auto s = db_->container_.db->Get(
        rocksdb::ReadOptions(),
        db_->container_.family(Family::ownershipSets),
        slice(EncodedNat(id).byteRange()),
        &val);
    binary::Input inp(byteRange(val));
    if (!s.IsNotFound()) {
      check(s);
      SetExpr<SetU32> exp;
      exp.op = static_cast<SetOp>(inp.trustedNat());
      OwnerSet efset;
      deserializeEliasFano(inp, efset);
      exp.set = SetU32::fromEliasFano(efset);
      return exp;
    } else {
      return folly::none;
    }
  }

  std::unique_ptr<rts::OwnershipSetIterator> getSetIterator() override {
    return db_->getSetIterator();
  }

 private:
  DatabaseImpl *db_;
};

const DatabaseImpl::FactOwnerCache::Page*
DatabaseImpl::FactOwnerCache::getPage(ContainerImpl& container, uint64_t prefix) {
  auto cachePtr = cache_.ulock();
  auto cache = cachePtr->get();

  if (!cache) {
      return nullptr;
  }

  if (prefix >= cache->size() || !(*cache)[prefix]) {
    auto page = std::make_unique<FactOwnerCache::Page>();
    size_t size = 0;

    std::unique_ptr<rocksdb::Iterator> iter(
        container.db->NewIterator(
            rocksdb::ReadOptions(),
            container.family(Family::factOwners)));
    EncodedNat key(prefix << 16);

    iter->Seek(slice(key.byteRange()));

    while (iter->Valid()) {
      binary::Input key(byteRange(iter->key()));
      rts::Id factId = Id::fromWord(key.trustedNat());
      binary::Input val(byteRange(iter->value()));
      UsetId usetId = val.trustedNat();
      if ((factId.toWord() >> 16) != prefix) {
        break;
      }
      page->factIds.push_back(factId.toWord() & 0xffff);
      page->setIds.push_back(usetId);
      iter->Next();
      size += sizeof(uint16_t) + sizeof(UsetId);
    }

    size_ += size;

    auto wlock = cachePtr.moveFromUpgradeToWrite();
    auto wcache = wlock->get();
    if (prefix >= wcache->size()) {
      wcache->resize(prefix+1);
    }
    (*wcache)[prefix] = std::move(page);

    VLOG(1) << "FactOwnerCache::getPage(" << prefix << ") size = " <<
      folly::prettyPrint(size_, folly::PRETTY_BYTES_IEC);
    return (*wcache)[prefix].get();
  }


  return (*cache)[prefix].get();
}

std::unique_ptr<rts::Ownership> DatabaseImpl::getOwnership() {
  container_.requireOpen();
  return std::make_unique<StoredOwnership>(this);
}

UsetId StoredOwnership::getOwner(Id id) {
  return db_->factOwnerCache_.getOwner(db_->container_, id);
}

void DatabaseImpl::FactOwnerCache::enable() {
  auto cache = cache_.ulock();
  if (*cache) return;

  auto wcache = cache.moveFromUpgradeToWrite();
  *wcache = std::make_unique<FactOwnerCache::Cache>();
  size_ = 0;
}

UsetId DatabaseImpl::FactOwnerCache::getOwner(ContainerImpl& container, Id id) {
  // first find the right page
  auto prefix = id.toWord() >> 16;
  const auto* page = getPage(container, prefix);

  if (!page) {
    // cache is not enabled; fall back to reading from the DB.
    std::unique_ptr<rocksdb::Iterator> iter(
        container.db->NewIterator(
            rocksdb::ReadOptions(),
            container.family(Family::factOwners)));

    EncodedNat key(id.toWord());
    iter->SeekForPrev(slice(key.byteRange()));
    if (iter->Valid()) {
      binary::Input val(byteRange(iter->value()));
      return val.trustedNat();
    } else {
      return INVALID_USET;
    }
  }

  // next binary-search on the content of the page to find the
  // interval containing the desired fact ID.
  uint32_t low, high, mid;
  uint16_t ix = id.toWord() & 0xffff;

  low = 0;
  high = page->factIds.size();

  // low is inclusive, high is exclusive
  // low..high always contains an element that is <= id, if there is one
  while (high - low > 1) {
    mid = (high + low) / 2;
    auto x = page->factIds[mid];
    if (x == ix) {
      return page->setIds[mid];
    }
    if (x < ix) {
      low = mid;
    } else {
      high = mid;
    }
  }

  if (page->factIds[low] <= ix) {
    return page->setIds[low];
  } else {
    // we have to look in a previous page
    auto prevPrefix = prefix;
    while (prevPrefix > 0) {
      prevPrefix--;
      const auto& prevPage = getPage(container, prevPrefix);
      if (prevPage->setIds.size() > 0) {
        return prevPage->setIds[prevPage->setIds.size() - 1];
      }
    }
    return INVALID_USET;
  }
}

void DatabaseImpl::cacheOwnership() {
  factOwnerCache_.enable();
}

void DatabaseImpl::addDefineOwnership(DefineOwnership& def) {
  auto t = makeAutoTimer("addDefineOwnership");
  container_.requireOpen();

  VLOG(1) << "addDefineOwnership: " <<
    def.owners_.size() + def.new_owners_.size() << " owners, " <<
    def.usets_.size() << " sets";

  if (def.newSets_.size() > 0) {

    folly::F14FastMap<UsetId,UsetId> substitution;
    auto subst = [&](uint32_t old) -> uint32_t {
      auto n = substitution.find(old);
      if (n == substitution.end()) {
        return old;
      } else {
        return n->second;
      }
    };

    rocksdb::WriteBatch batch;
    size_t numNewSets = 0;

    for (auto uset : def.newSets_) {
      std::set<UsetId> s;
      uset->exp.set.foreach([&](uint32_t elt) {
        s.insert(subst(elt));
      });
      SetU32 set = SetU32::from(s);

      auto newUset = std::make_unique<Uset>(std::move(set), uset->exp.op, 0);
      auto p = newUset.get();
      auto oldId = uset->id;
      auto q = usets_->add(std::move(newUset));
      if (p == q) {
        usets_->promote(p);
        auto ownerset = p->toEliasFano();
        putOwnerSet(batch, p->id, ownerset.op, ownerset.set);
        ownerset.set.free();
        numNewSets++;
      }
      VLOG(2) << "rebased set " << oldId << " -> " << q->id;
      substitution[oldId] = q->id;
    }
    VLOG(1) << "addDefineOwnership: writing sets (" << numNewSets << ")";
    check(container_.db->Write(container_.writeOptions, &batch));

    for (auto& owner : def.owners_) {
      owner = subst(owner);
    }
  }

  // ownershipDerivedRaw :: (Pid,nat) -> vector<int64_t>
  //
  // Similarly to ownershipRaw, this is basically just an
  // append-only log. The nat in the key is a per-Pid counter that
  // we bump by one each time we add another batch of data for a
  // Pid.

  binary::Output key;
  key.nat(def.pid_.toWord());
  const auto [it, _] = ownership_derived_counters.insert(
      {def.pid_.toWord(), 0});
  key.nat(it->second++);

  rocksdb::WriteBatch batch;

  binary::Output val;

  val.bytes(
      def.ids_.data(),
      def.ids_.size() *
      sizeof(std::remove_reference<decltype(def.ids_)>::type::value_type));
  val.bytes(
      def.new_ids_.data(),
      def.new_ids_.size() *
      sizeof(std::remove_reference<decltype(def.new_ids_)>::type::value_type));
  val.bytes(
      def.owners_.data(),
      def.owners_.size() *
      sizeof(std::remove_reference<decltype(def.owners_)>::type::value_type));
  val.bytes(
      def.new_owners_.data(),
      def.new_owners_.size() *
      sizeof(std::remove_reference<decltype(def.new_owners_)>::type::value_type));

  check(batch.Put(
            container_.family(Family::ownershipDerivedRaw),
            slice(key),
            slice(val)));

  check(container_.db->Write(container_.writeOptions, &batch));

  VLOG(1) << "addDefineOwnership wrote " <<
    def.ids_.size() + def.new_ids_.size() <<
    " entries for pid " << def.pid_.toWord();
}

std::unique_ptr<rts::OwnershipSetIterator>
  DatabaseImpl::getSetIterator() {
  struct SetIterator : rts::OwnershipSetIterator {
    explicit SetIterator(size_t first, size_t size,
                         std::unique_ptr<rocksdb::Iterator> i)
        : first_(first), size_(size), iter(std::move(i)) {}

    folly::Optional<std::pair<UsetId, SetExpr<const OwnerSet *>>> get()
      override {
      if (iter->Valid()) {
        binary::Input key(byteRange(iter->key()));
        auto usetid = key.trustedNat();
        binary::Input val(byteRange(iter->value()));
        iter->Next();
        exp.op = static_cast<SetOp>(val.trustedNat());
        deserializeEliasFano(val, exp.set);
        return std::pair<uint32_t, SetExpr<const OwnerSet *>>(
            usetid, { exp.op, &exp.set });
      } else {
        return folly::none;
      }
    }

    std::pair<size_t,size_t> sizes() const override {
      return { first_, size_ };
    }

    SetExpr<OwnerSet> exp;
    size_t first_, size_;
    std::unique_ptr<rocksdb::Iterator> iter;
  };

  std::unique_ptr<rocksdb::Iterator> iter(container_.db->NewIterator(
      rocksdb::ReadOptions(),
      container_.family(Family::ownershipSets)));

  if (!iter) {
    rts::error("rocksdb: couldn't allocate ownership set iterator");
  }

  size_t first, last, size;

  iter->SeekToLast();
  if (!iter->Valid()) {
    last = 0;
  } else {
    binary::Input key(byteRange(iter->key()));
    last = key.trustedNat();
  }

  iter->SeekToFirst();
  if (!iter->Valid()) {
    first = 0;
    size = 0;
  } else {
    binary::Input key(byteRange(iter->key()));
    first = key.trustedNat();
    size = last - first + 1;
  }

  return std::make_unique<SetIterator>(
      first, size, std::move(iter));
}

std::unique_ptr<Database> ContainerImpl::openDatabase(
    Id start, int32_t version) && {
  return std::make_unique<DatabaseImpl>(std::move(*this), start, version);
}

}

std::unique_ptr<Container> open(
    const std::string& path,
    Mode mode,
    bool cache_index_and_filter_blocks,
    folly::Optional<std::shared_ptr<Cache>> cache) {
  return std::make_unique<ContainerImpl>(path, mode, cache_index_and_filter_blocks, std::move(cache));
}

std::shared_ptr<Cache> newCache(size_t capacity) {
  return rocksdb::NewLRUCache(capacity);
}

void restore(const std::string& target, const std::string& source) {
  check(
    ContainerImpl::backupEngine(source)
    ->RestoreDBFromLatestBackup(target,target));
}

}
}
}
