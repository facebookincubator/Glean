// Copyright (c) Facebook, Inc. and its affiliates.

#include <fstream>
#include <mhash.h>
#include <endian.h>

#include "glean/client/cpp/GleanClient.h"
#include "common/config/SimpleConfigeratorConfig.h"
#include "common/network/NetworkUtil.h"
#include "common/base/BuildInfo.h"
#include "common/base/Unixname.h"

using namespace facebook;
using namespace facebook::config;
using namespace facebook::network;
using namespace facebook::servicerouter;
using namespace facebook::servicerouter::cpp2;

namespace facebook {
namespace glean {

static const auto configName = "glean/client/client";

Glean::Glean()
    : factory_(getClientFactory()) {
  auto conf = new ConfigeratorConfig<ClientConfig>(configName);
  config_ = std::shared_ptr<ConfigeratorConfig<ClientConfig>>(conf);
}

Glean::Glean(ClientConfig config)
    : factory_(getClientFactory()) {
  auto conf_ = std::make_shared<ClientConfig>(std::move(config));
  config_ = std::make_shared<SimpleConfigeratorConfig<ClientConfig>>(conf_);
}

std::unique_ptr<thrift::GleanServiceAsyncClient> Glean::getClient(
    folly::Optional<Shard> shard) {
  auto clientParams = facebook::servicerouter::ClientParams();

  auto serv = config_->get()->get_serv();
  std::string tier = "";
  if (serv.getType() == Service::Type::hostPort) {
    auto& hp = serv.get_hostPort();
    clientParams.setSingleHost(
        NetworkUtil::getHostByName(*hp.host_ref()), *hp.port_ref());
    // An smc tier will get timeouts via the tier config, but when
    // using a specific host/port we need to set longer timeouts.
    if (config_->get()->host_timeout_ms_ref().is_set()) {
      clientParams.setSockConnTimeoutMs_DEPRECATED(
          std::chrono::milliseconds(config_->get()->get_host_timeout_ms()));
      clientParams.setProcessingTimeoutMs(
          std::chrono::milliseconds(config_->get()->get_host_timeout_ms()));
    }
  } else {
    tier = serv.get_tier();
  }
  if (shard && *config_->get()->use_shards_ref() != UseShards::NO_SHARDS) {
    clientParams.setShardId(shard.value());
  };
  return factory_.getSRClientUnique<thrift::GleanServiceAsyncClient>(
      tier, clientParams);
}

std::string Glean::getShard(thrift::Repo repo) {
  MHASH td;

  string str = *repo.name_ref() + "/" + *repo.hash_ref();

  td = mhash_init(MHASH_MD5);
  if (td == MHASH_FAILED) {
    throw std::runtime_error("mhash_init failed");
  }

  mhash(td, str.data(), str.size());

  int block_size = mhash_get_block_size(MHASH_MD5);
  char *hash = new char[block_size];
  bzero(hash, block_size);

  // Free resources
  mhash_deinit(td, hash);

  uint64_t w = be64toh(*reinterpret_cast<uint64_t*>(hash)) >> 1;
  delete[] hash;

  return folly::to<std::string>(w);
}

Glean::Repo Glean::getLatestRepo(std::string repoName) {
  auto client = getClient();
  thrift::ListDatabasesResult res;
  client->sync_listDatabases(res, thrift::ListDatabases{});

  folly::Optional<thrift::Database> latest;
  for (auto& db : *res.databases_ref()) {
    auto status = db.status_ref();
    auto created = db.created_since_epoch_ref();
    auto expire = db.expire_time_ref();
    int64_t now =
      ticks_since_epoch<std::chrono::system_clock, std::chrono::seconds>();
    auto minDbAge = *config_->get()->min_db_age_ref();
    if (*db.repo_ref()->name_ref() == repoName && status &&
        *status == thrift::DatabaseStatus::Complete && !expire && created &&
        (now - *created > minDbAge) &&
        (!latest || !latest->created_since_epoch_ref() ||
         *(latest->created_since_epoch_ref()) < *created)) {
      latest = db;
    }
  }

  if (latest) {
    return *latest->repo_ref();
  } else {
    throw std::runtime_error("no db available for " + repoName);
  }
}

std::string Glean::predicateFromType(const std::string& ty) {
  if (!boost::starts_with(ty, SCHEMA_NAMESPACE)) {
    throw std::runtime_error(
        ty + " is not a type in " SCHEMA_NAMESPACE );
  }
  auto name = ty.substr(sizeof(SCHEMA_NAMESPACE)-1);
  boost::replace_all(name, "::", ".");
  return name;
}

thrift::UserQueryClientInfo getClientInfo() {
  auto info = thrift::UserQueryClientInfo();
  info.unixname_ref() = getUserUnixname();
  info.application_ref() = BuildInfo::getBuildRule();
  info.name_ref() = "api-c++";
  return info;
}

GleanRepo::GleanRepo(Repo repo)
  : repo_(repo), shard_(Glean::getShard(repo)), clientInfo_(getClientInfo()) {};

GleanRepo::GleanRepo(Repo repo, std::shared_ptr<Glean> glean)
  : glean_(std::move(glean)), repo_(repo), shard_(Glean::getShard(repo)),
    clientInfo_(getClientInfo()) {}

GleanRepo::GleanRepo(Repo repo, ClientConfig config)
  : glean_(std::make_shared<Glean>(config)), repo_(repo),
    clientInfo_(getClientInfo()) {
  shard_ = Glean::getShard(repo);
}

GleanRepo::GleanRepo(std::string repoName)
  : glean_(std::make_shared<Glean>()), clientInfo_(getClientInfo()) {
  repo_ = glean_->getLatestRepo(repoName);
  shard_ = Glean::getShard(repo_);
}

GleanRepo::GleanRepo(std::string repoName, ClientConfig config)
    : glean_(std::make_shared<Glean>(config)), clientInfo_(getClientInfo()) {
  repo_ = glean_->getLatestRepo(repoName);
  shard_ = Glean::getShard(repo_);
}

GleanRepo::GleanRepo(std::string repoName, std::shared_ptr<Glean> glean)
    : glean_(std::move(glean)), clientInfo_(getClientInfo()) {
  repo_ = glean_->getLatestRepo(repoName);
  shard_ = Glean::getShard(repo_);
}

std::vector<std::string> GleanRepo::queryJson(
    const std::string& predicate,
    const std::string& query,
    thrift::UserQueryOptions opts) {
  auto client = getClient();

  opts.expand_results_ref() = true;

  auto req = thrift::UserQuery{};
  req.predicate_ref() = predicate;
  req.query_ref() = query;
  req.options_ref() = opts;
  req.client_info_ref() = clientInfo_;

  auto res = thrift::UserQueryResults{};
  client->sync_userQuery(res, repo_, req);

  return *res.facts_ref();
}

std::vector<std::string> GleanRepo::queryJson(
    const std::string& predicate,
    Version predicateVersion,
    const std::string& query,
    thrift::UserQueryOptions opts) {
  auto client = getClient();

  opts.expand_results_ref() = true;

  auto req = thrift::UserQuery{};
  req.predicate_ref() = predicate;
  req.predicate_version_ref() = predicateVersion;
  req.query_ref() = query;
  req.options_ref() = opts;
  req.client_info_ref() = clientInfo_;

  auto res = thrift::UserQueryResults{};
  client->sync_userQuery(res, repo_, req);

  return *res.facts_ref();
}

} // namespace glean
} // namespace facebook
