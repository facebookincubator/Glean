/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <csignal>
#include <cstdlib>
#include <iostream>
#include <string>
#include <filesystem>

#include "clang/Basic/DiagnosticOptions.h"
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/ErrorHandling.h>

#include <boost/algorithm/string/predicate.hpp>

#include <folly/Conv.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/FileUtil.h>
#include <folly/json.h>
#include <folly/Range.h>

#if FACEBOOK
#include "common/init/Init.h"
#else
#include <folly/init/Init.h>
#endif

#include "thrift/lib/cpp/transport/TTransportException.h"

#include "glean/cpp/glean.h"
#include "glean/cpp/sender.h"
#include "glean/interprocess/cpp/counters.h"
#include "glean/interprocess/cpp/worklist.h"
#include "glean/lang/clang/action.h"
#include "glean/lang/clang/ast.h"
#include "glean/lang/clang/gleandiagnosticbuffer.h"
#include "glean/lang/clang/preprocessor.h"
#include "glean/rts/binary.h"
#include "glean/rts/inventory.h"

DEFINE_string(service, "", "TIER or HOST:PORT of Glean write server. When specified the generated facts will be sent to that server. You MUST specify either --dump or --service, but not both.");
DEFINE_string(dump, "", "PATH where generated facts will be dumped instead of sending them to the Glean write server. You MUST specify either --dump or --service, but not both.");
DEFINE_string(work_file, "", "PATH to work file");
DEFINE_string(task, "", "task id (for logging)");
DEFINE_string(request, "", "request id (for logging)");
DEFINE_string(origin, "", "origin (for logging)");
DEFINE_string(inventory, "", "PATH to inventory file");
DEFINE_string(root, ".", "root repository PATH");
DEFINE_string(blank_cell_name, "", "buck cell name output as nothing");
DEFINE_string(cwd_subdir, "", "current working subdirectory under --root");
DEFINE_string(target_subdir, "", "clang target subdirectory under --root");
DEFINE_string(path_prefix, "",
  "Path fragment to prefix src.File facts with");
DEFINE_string(repo_name, "", "Glean database name (formely known as repo). Used in logging. Also when --service is specified, 'repo_name/repo_hash' identifies the glean DB to write to.");
DEFINE_string(repo_hash, "", "Glean database instance (formely known as hash). Used in logging. Also when --service is specified, 'repo_name/repo_hash' identifies the glean DB to write to.");
DEFINE_int32(max_comm_errors, 30,
  "maximum number of consecutive communication errors");
DEFINE_int32(stop_after, 0, "stop after N files");
DEFINE_int32(max_rss, 6291456, "stop after RSS reaches this size (kB)");
DEFINE_bool(dry_run, false, "don't send data");
DEFINE_bool(fact_stats, true, "log fact statistics");
DEFINE_uint64(fact_cache, 805306368, "set maximum fact cache size");
DEFINE_uint64(fact_buffer, 201326592, "set maximum fact buffer size");
DEFINE_uint32(log_every, 1, "log every N translation units");
DEFINE_uint32(worker_index, 0, "index of this worker");
DEFINE_uint32(worker_count, 1, "total number of workers");
DEFINE_string(counter_file, "", "PATH to stats counter file");
DEFINE_string(counters, "", "comma-separated list of NAME@N");
DEFINE_bool(suppress_diagnostics, false, "suppress all Clang diagnostics");
DEFINE_uint32(
    max_diagnostics_size,
    10,
    "max number of diagnostics to log in IndexFailure predicate");
DEFINE_bool(fail_on_error, false, "immediately fail on compilation errors");
DEFINE_bool(index_on_error, false, "index files that have compilation errors");
DEFINE_string(clang_arguments, "", "arguments to pass to Clang");
DEFINE_bool(clang_no_pch, false, "disable PCH");
DEFINE_bool(clang_no_modules, false, "disable modules");
DEFINE_string(clang_resource_dir, "", "PATH to Clang resource dir");

// Index single cdb
DEFINE_string(cdb_target, "", "Target name");
DEFINE_string(cdb_dir, "", "Directory with compile_commands.json in it");

// This is a hack to support parallel indexing in the Glean CLI
DEFINE_bool(
    print_sources_count,
    false,
    "Print the number source files and exit");

static llvm::cl::OptionCategory indexerCategory("glean");

// This file implements some plumbing and the main function for the
// Clang indexer

namespace {

using namespace facebook::glean;
using namespace facebook::glean::clangx;

#define LOG_CFG(level,config) LOG(level) << (config).log_pfx

struct Counters {
  using counter_t = interprocess::Counters::counter_t;

  Counters() {
    std::vector<std::pair<std::string, size_t>> names;

    if (!FLAGS_counter_file.empty()) {
      size_t size = 0;
      const auto spec = FLAGS_counters;
      const auto end = spec.end();
      auto pos = spec.begin();
      while (pos != end) {
        auto a = std::find(pos,end,'@');
        if (a == end) {
          throw std::runtime_error("invalid --counter");
        }
        auto b = a+1;
        auto e = std::find(b,end,',');
        auto index = folly::to<size_t>(std::string(b,e));
        size = std::max(size, index+1);
        names.push_back({std::string(pos,a), index});
        pos = e;
        if (pos != end) {
          ++pos;
        }
      }

      counters = interprocess::counters(FLAGS_counter_file, size);
    }

    const std::unordered_map<std::string, counter_t**> fields{
      {"fact_buffer_size", &fact_buffer_size},
      {"fact_cache_size", &fact_cache_size},
      {"fact_cache_hits", &fact_cache_hits},
      {"fact_cache_misses", &fact_cache_misses},
    };

    for (const auto& x : fields) {
      *(x.second) = nullptr;
    }

    for (const auto& x : names) {
      auto p = fields.find(x.first);
      if (p != fields.end()) {
        *(p->second) = counters->counter(x.second);
      } else {
        LOG(ERROR) << "unknown counter '" << x.first << "'";
      }
    }

    for (const auto& x : fields) {
      if (*(x.second) == nullptr) {
        locals.emplace_back(0);
        *(x.second) = &locals.back();
      }
    }
  }

  counter_t *fact_buffer_size;
  counter_t *fact_cache_size;
  counter_t *fact_cache_hits;
  counter_t *fact_cache_misses;
  std::unique_ptr<interprocess::Counters> counters;
  std::deque<std::atomic<uint64_t>> locals;
};

struct Config {
  std::filesystem::path root;
  // subdir of root for setting current working directory
  folly::Optional<std::string> cwd_subdir;
  // subdir of root for interpreting relative clang paths
  folly::Optional<std::string> target_subdir;
  folly::Optional<std::string> path_prefix;
  folly::Optional<std::string> platform;
  std::string log_pfx;

  std::unique_ptr<Sender> sender;
  bool should_log;

  std::unique_ptr<DbSchema<SCHEMA>> schema;

  std::unique_ptr<GleanDiagnosticBuffer> diagnostics;

  std::vector<SourceFile> sources;

  Counters counters;

  Config(int argc, char **argv) {
    assert (argc > 0);
    root = std::filesystem::canonical(FLAGS_root);
    if (!FLAGS_cwd_subdir.empty()) {
      cwd_subdir = FLAGS_cwd_subdir;
    }
    if (!FLAGS_target_subdir.empty()) {
      target_subdir = FLAGS_target_subdir;
    }
    if (!FLAGS_path_prefix.empty()) {
      path_prefix = FLAGS_path_prefix;
    }
    log_pfx = folly::to<std::string>(FLAGS_worker_index) + ": ";

    #if FACEBOOK
    if (!FLAGS_service.empty()) {
      // Full logging if we are talking to a remote service
      should_log = true;

      if (FLAGS_repo_name.empty()) {
        fail("missing repo_name");
      }
      if (FLAGS_repo_hash.empty()) {
        fail("missing repo_hash");
      }

      sender = thriftSender(
        FLAGS_service,
        FLAGS_repo_name,
        FLAGS_repo_hash,
        10,          // hardcode min_retry_delay for now
        static_cast<size_t>(FLAGS_max_comm_errors)
      );
    } else
      #endif
      if (!FLAGS_dump.empty()) {
      // No logging when dumping to a file
      should_log = false;
      sender = fileWriter(FLAGS_dump);
    } else if (!FLAGS_print_sources_count) {
      fail("missing --service or --dump");
    }

    if (FLAGS_inventory.empty()) {
      fail("missing --inventory");
    } else {
      std::string contents;
      if (!folly::readFile(FLAGS_inventory.c_str(), contents)) {
        fail("couldn't read " + FLAGS_inventory);
      }

      schema = std::make_unique<DbSchema<SCHEMA>>(
        rts::Inventory::deserialize(binary::byteRange(contents)));
    }

    diagnostics = diagnosticConsumer();

    // Add targets from json
    for (int i = 1; i < argc; ++i) {
      std::string contents;
      if (!folly::readFile(argv[i], contents)) {
        fail(std::string("couldn't read ") + argv[i]);
      }

      for (const auto& item : folly::parseJson(contents)) {
        folly::Optional<std::string> platform;
        if (auto *p = item.get_ptr("platform")) {
          platform = p->getString();
        }
        sources.push_back(SourceFile{
          item["target"].getString(),
          std::move(platform),
          item["dir"].getString(),
          item["file"].getString()});
      }
    }

    // Add sources from single cdb options
    if (!FLAGS_cdb_target.empty() || !FLAGS_cdb_dir.empty()) {
      if (FLAGS_cdb_target.empty()) {
        fail("missing --cdb-target");
      }
      if (FLAGS_cdb_dir.empty()) {
        fail("missing --cdb-dir");
      }
      std::string err;
      std::string dir = FLAGS_cdb_dir.c_str();
      auto cdb = clang::tooling::CompilationDatabase::loadFromDirectory(dir, err);
      if (!cdb) {
        throw std::runtime_error("couldn't load " + dir + ": " + err);
      }
      for(auto file : cdb->getAllFiles()){
        sources.push_back(SourceFile{
            FLAGS_cdb_target.c_str(),
            folly::Optional<std::string>(),
            FLAGS_cdb_dir.c_str(),
            file,
        });
      }
    }
  }

  ActionLogger logger(const std::string &name) {
    return ActionLogger(name,
                        FLAGS_task,
                        FLAGS_request,
                        FLAGS_repo_name,
                        FLAGS_repo_hash,
                        FLAGS_worker_index,
                        FLAGS_origin,
                        FLAGS_cwd_subdir,
                        should_log);
  }

  [[noreturn]] void fail(const std::string& msg) const {
    LOG_CFG(FATAL, *this) << msg;
  }

  static std::unique_ptr<GleanDiagnosticBuffer> diagnosticConsumer() {
    if (FLAGS_suppress_diagnostics) {
      return std::make_unique<GleanDiagnosticBuffer>(
          FLAGS_max_diagnostics_size);
    } else {
      // Forward the diagnostics to TextDiagnosticPrinter which prints
      // the diagnostics to standard error.
      return std::make_unique<GleanDiagnosticBuffer>(
          FLAGS_max_diagnostics_size,
          std::make_unique<clang::TextDiagnosticPrinter>(
              llvm::errs(), new clang::DiagnosticOptions{}));
    }
  }
};

// A wrapper for a Clang compilation database
class CDB {
public:
  // Load the compilation database for a particular source file if it is
  // different from the one loaded before. The pointer is valid until the
  // next call to load as long as the CDB object is alive.
  const clang::tooling::CompilationDatabase *load(const SourceFile& source) {
    if (!cdb || dir != source.dir) {
      std::string err;
      dir = source.dir;
      cdb = clang::tooling::CompilationDatabase::loadFromDirectory(dir, err);
      if (!cdb) {
        throw std::runtime_error("couldn't load " + dir + ": " + err);
      }
    }
    return cdb.get();
  }

private:
  std::unique_ptr<clang::tooling::CompilationDatabase> cdb;
  std::string dir;
};

struct SourceIndexer {
  const Config& config;
  Batch<SCHEMA> batch;
  CDB cdb;

  explicit SourceIndexer(Config& cfg)
    : config(cfg)
    , batch(cfg.schema.get(), FLAGS_fact_cache)
    {
      blank_cell_name = (!FLAGS_blank_cell_name.empty())
        ? folly::Optional<std::string>(FLAGS_blank_cell_name)
        : folly::none;
    }

  bool index(const SourceFile& source) {
    auto pcdb = cdb.load(source);
    ClangCfg cfg{
      ClangDB::Env{
        locatorOf(source),
        platformOf(source),
        config.root,
        config.target_subdir,
        config.path_prefix,
        batch,
      },
      config.diagnostics.get()
    };
    FrontendActionFactory factory(&cfg);
    clang::tooling::ClangTool tool(*pcdb, source.file);
    if (!FLAGS_clang_arguments.empty()) {
      clang::tooling::CommandLineArguments args;
      folly::split(" ", FLAGS_clang_arguments, args, true);
      if (!args.empty()) {
        tool.appendArgumentsAdjuster(
          clang::tooling::getInsertArgumentAdjuster(
            args, clang::tooling::ArgumentInsertPosition::END));
      }
    }
    tool.appendArgumentsAdjuster([](const auto& args, auto) {
      clang::tooling::CommandLineArguments stripped;
      stripped.reserve(args.size());
      for (size_t i = 0; i < args.size(); ++i) {
        if (FLAGS_clang_no_pch && args[i] == "-include-pch") {
          ++i;  // skip next argument
        } else if (FLAGS_clang_no_pch
                   && args[i] == "-include"
                   && i+1 < args.size()
                   && boost::ends_with(args[i+1],".pch")) {
          // headers included from the command line cause problems
          // because they won't be visible to tools exploring the
          // include graph via the cxx.Trace predicate. When used to
          // include a .pch file these are typically a compile-time
          // optimisation only, so we strip them out when
          // --clang_no_pch is on.
          ++i;
        } else if (!FLAGS_clang_resource_dir.empty()
                    && args[i] == "-resource-dir") {
          ++i;  // replace -resource-dir flag
          stripped.push_back("-resource-dir");
          stripped.push_back(FLAGS_clang_resource_dir);
        } else if (boost::starts_with(args[i], "--cc=")) {
          // skip this flag - llvm complains about it
        } else if (FLAGS_clang_no_modules &&
                   (args[i] == "-fmodules"
                    || args[i] == "-fcxx-modules"
                    || boost::starts_with(args[i], "-fmodule-name="))) {
          // skip these
        } else {
          stripped.push_back(args[i]);
        }
      }
      return stripped;
    });
    return tool.run(&factory) == 0;
  }

private:
  folly::Optional<std::string> blank_cell_name;

  Fact<Buck::Locator> locatorOf(const SourceFile& source) {
    // Parsing source.target as cell//path:name
    const auto slashes = source.target.find("//");
    const size_t cell_len = (slashes == std::string::npos) ? 0 : slashes;
    const size_t path_start = (slashes == std::string::npos) ? 0 : slashes+2;

    const auto colon = source.target.find(':', path_start);
    const size_t path_len = (colon == std::string::npos)
      ? source.target.size() - path_start
      : colon - path_start;
    const size_t name_start = (colon == std::string::npos)
      ? source.target.size()  // substr will return empty string for name
      : colon+1;

    folly::Optional<std::string> cell = (0 < cell_len)
      ? folly::Optional<std::string>(source.target.substr(0, cell_len))
      : folly::none;

    // Enforce policy that buck.Locator{subdir=nothing} means blank_cell_name
    if (cell == blank_cell_name) {
      cell = folly::none;
    }

    return batch.fact<Buck::Locator>(
      maybe(cell),
      source.target.substr(path_start, path_len),
      source.target.substr(name_start)
    );
  }

  folly::Optional<Fact<Buck::Platform>> platformOf(
      const SourceFile& file) {
    if (file.platform) {
      return batch.fact<Buck::Platform>(file.platform.value());
    } else {
      return folly::none;
    }
  }

  struct ClangCfg {
    ClangDB::Env env;
    GleanDiagnosticBuffer *diagnostics;
  };

  // FrontendAction uses the ClangIndexer to plumb PPCallbacks and ASTConsumer
  struct FrontendAction : public clang::ASTFrontendAction {
    using Base = clang::ASTFrontendAction;
    explicit FrontendAction(const ClangCfg *cfg) : config(cfg) {}

    bool PrepareToExecuteAction(clang::CompilerInstance& ci) override {
      if (config->diagnostics) {
        ci.getDiagnostics().setClient(config->diagnostics, false);
      }
      return clang::ASTFrontendAction::PrepareToExecuteAction(ci);
    }

    bool BeginSourceFileAction(clang::CompilerInstance& ci) override {
      if (config->diagnostics) {
        config->diagnostics->clear();
      }
      db = std::make_unique<ClangDB>(config->env, ci, config->diagnostics);
      ci.getPreprocessor().addPPCallbacks(
          facebook::glean::clangx::newPPCallbacks(db.get()));
      return Base::BeginSourceFileAction(ci);
    }

    std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
        clang::CompilerInstance&, clang::StringRef) override {
      CHECK(db);
      return facebook::glean::clangx::newASTConsumer(db.get());
    }

    const ClangCfg *config;
    std::unique_ptr<ClangDB> db;
  };

  struct FrontendActionFactory : public clang::tooling::FrontendActionFactory {
    explicit FrontendActionFactory(const ClangCfg *cfg) : config(cfg) {}

#if LLVM_VERSION_MAJOR >= 11
    std::unique_ptr<clang::FrontendAction> create() override {
      return std::unique_ptr<clang::FrontendAction>(new FrontendAction(config));
    }
#else
    clang::FrontendAction *create() override {
      return new FrontendAction(config);
    }
#endif

    const ClangCfg *config;
  };
};

using stats_vec = std::vector<std::pair<size_t, const char *>>;
const stats_vec counts = {{1000*1000, "m"}, {1000, "k"}};
const stats_vec mems = {{1024*1024, "MB"}, {1024, "KB"}, {0, "B"}};

folly::fbstring showStat(const stats_vec& vec, size_t n) {
  for (const auto& p : vec) {
    if (n >= p.first) {
      if (p.first != 0) {
        n /= p.first;
      }
      return folly::to<folly::fbstring>(n) + p.second;
    }
  }
  return folly::to<folly::fbstring>(n);
}

folly::fbstring showStats(const FactStats& stats) {
  return
    showStat(mems, stats.memory) + " (" + showStat(counts, stats.count) + ")";
}

struct FatalLLVMError : std::runtime_error {
  using std::runtime_error::runtime_error;
};

#if LLVM_VERSION_MAJOR > 13
void handleLLVMError(void *, const char* reason, bool) {
#else
void handleLLVMError(void *, const std::string& reason, bool) {
#endif
  throw FatalLLVMError(reason);
}

int getSelfRSS() {
  std::string contents;
  if (!folly::readFile("/proc/self/smaps_rollup", contents)) {
    LOG(ERROR) << "Couldn't read /proc/self/smaps_rollup";
    return 0;
  }
  std::vector<std::string> lines;
  folly::split("\n", contents, lines);
  for (const auto& line : lines) {
    folly::StringPiece piece(line);
    if (piece.subpiece(0, 4) == "Rss:") {
      auto rss = piece.subpiece(4);
      return folly::to<int>(&rss);
    }
  }
  return 0;
}

}

int main(int argc, char **argv) {
#if FACEBOOK
  facebook::initFacebook(&argc, &argv);
#else
  folly::init(&argc, &argv);
#endif

  std::signal(SIGTERM, [](int) {
    #if FACEBOOK
    LOG(CRITICAL)
    #else
    LOG(ERROR)
    #endif
      << "worker " << FLAGS_worker_index << " received SIGTERM, exiting";
    _exit(1);
  });

  Config config(argc, argv);

  if (FLAGS_print_sources_count) {
    std::cout << config.sources.size();
    return 0;
  }

  const auto work_counter = FLAGS_work_file.empty()
    ? worklist::serialCounter(0, config.sources.size())
    : worklist::stealingCounter(
        FLAGS_work_file, FLAGS_worker_index, FLAGS_worker_count);

  SourceIndexer indexer(config);

  llvm::install_fatal_error_handler(&handleLLVMError, nullptr);

  const size_t n = FLAGS_stop_after != 0
    ? std::min(size_t(FLAGS_stop_after), config.sources.size())
    : config.sources.size();

  FactStats prev_stats = {0,0};
  FactStats lifetime_stats = {0,0};
  uint32_t lifetime_files = 0;

  bool error_exit = false;
  bool memory_exit = false;
  int rss = 0;
  for (auto next = work_counter->next();
        next.has_value();
        next = work_counter->next()) {
    const auto i = next.value().start;
    auto errorGuard = folly::makeGuard([&] {
      LOG_CFG(ERROR,config) << "error guard at "
        << i+1 << "/" << next.value().end << " [" << n << "] "
        << config.sources[i].file;
    });

    if (FLAGS_log_every != 0 && (lifetime_files % FLAGS_log_every) == 0) {
      LOG_CFG(INFO,config)
        << i+1 << "/" << next.value().end << " [" << n << "] "
        << config.sources[i].file;
      if (FLAGS_fact_stats) {
        LOG_CFG(INFO,config)
          << "fact buffer: " << showStats(indexer.batch.bufferStats())
          << " cache: " << showStats(indexer.batch.cacheStats().facts)
          << " lifetime: " << showStats(lifetime_stats);
      }
    }

    const auto& source = config.sources[i];
    const auto buf_stats = indexer.batch.bufferStats();
    const auto cache_stats = indexer.batch.cacheStats();
    try {
      bool ok = config
        .logger("clang/index")
        .log_index(source, buf_stats, cache_stats, [&]() {
          return indexer.index(source);
        });
      if (!ok && FLAGS_fail_on_error) {
        LOG(ERROR) << "compilation failed for " << source.file;
        error_exit = true;
      }
    } catch(const FatalLLVMError& e) {
      // TODO: log this to Scuba if it turns out to happen a lot
      LOG(ERROR) << "fatal LLVM error in " << source.file << ": " << e.what();
      error_exit = true;
    } catch(const std::exception& e) {
      LOG(ERROR) << "while indexing " << source.file << ": " << e.what();
      error_exit = FLAGS_fail_on_error;
    }

    lifetime_stats.memory += buf_stats.memory - prev_stats.memory;
    lifetime_stats.count += buf_stats.count - prev_stats.count;
    config.counters.fact_buffer_size->store(buf_stats.memory);
    config.counters.fact_cache_size->store(cache_stats.facts.memory);
    config.counters.fact_cache_hits->store(cache_stats.hits);
    config.counters.fact_cache_misses->store(cache_stats.misses);
    if (!FLAGS_dry_run) {
      const bool wait =
        FLAGS_fact_buffer != 0 && buf_stats.memory >= FLAGS_fact_buffer;
      const auto start = std::chrono::steady_clock::now();
      if (wait) {
        LOG_CFG(INFO,config)
          << "fact buffer size " << buf_stats.memory << ", waiting";
      }
      config.logger(wait ? "clang/wait" : "clang/send").log([&]() {
        config.sender->rebaseAndSend(indexer.batch.base(), wait);
      });
      if (wait) {
        const auto wait_time = std::chrono::steady_clock::now() - start;
        LOG_CFG(INFO,config)
          << "rebaseAndSend wait time in milliseconds="
          << std::chrono::duration_cast<std::chrono::milliseconds>(wait_time).count();
      }
    }
    prev_stats = indexer.batch.bufferStats();
    ++lifetime_files;

    memory_exit = FLAGS_max_rss != 0 && (rss = getSelfRSS()) > FLAGS_max_rss;

    errorGuard.dismiss();

    if ((FLAGS_stop_after != 0 && lifetime_files >= FLAGS_stop_after)
        || memory_exit
        || error_exit) {
      LOG_CFG(WARNING,config)
        << "Exiting after "
        << i+1 << "/" << next.value().end << " [" << n << "] "
        << config.sources[i].file;
      // we do not want the for loop to call work_counter->next()
      // because that will skip the next target for no good reason
      break;
    }
  }

  if (!FLAGS_dry_run) {
    LOG_CFG(INFO,config) << "flushing";
    config.logger("clang/flush").log([&]() {
      config.sender->flush(indexer.batch.base());
    });
  }

  config.counters.fact_buffer_size->store(0);
  config.counters.fact_cache_size->store(0);

  if (memory_exit) {
    LOG_CFG(ERROR, config)
      << "Exiting due to memory pressure, RSS was " << rss
      << " kB, RSS after flushing is " << getSelfRSS()
      << " kB, --max-rss is " << FLAGS_max_rss << " kB";
  }

  LOG_CFG(INFO,config)
    << (error_exit || memory_exit ? "aborting" : "finished")
    << ", lifetime files: " << lifetime_files
    << " facts: " << showStats(lifetime_stats);

  if (memory_exit) {
    return 147;
  }
  if (error_exit) {
    return 1;
  }
  return 0;
}
