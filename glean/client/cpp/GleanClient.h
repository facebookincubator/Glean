#pragma once

#include <boost/type_index.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <string>

#include "common/config/ConfigeratorConfig.h"
#include "configerator/structs/glean/client/gen-cpp2/client_config_types.h"
#include "glean/if/gen-cpp2/GleanServiceAsyncClient.h"
#include "glean/if/gen-cpp2/glean_types.h"
#include "servicerouter/client/cpp2/ServiceRouter.h"
#include "thrift/lib/cpp2/protocol/Serializer.h"

namespace facebook {
namespace glean {

#define SCHEMA_NAMESPACE "facebook::glean::schema::"

/**
 * A generic Glean Client
 */
class Glean {
 public:
  using Repo = thrift::Repo;
  using Shard = std::string;

  /**
   * Initialize the Glean client, getting the configuration from
   * Configerator.
   */
  explicit Glean();

  /**
   * Initialize the Glean client, using the supplied configuration.
   * This can be used to override the host/port of the Glean server to
   * connect to, for example.
   */
  explicit Glean(ClientConfig config);

  Glean(const Glean&) = delete;
  Glean(Glean&&) = delete;
  Glean& operator=(const Glean&) = delete;
  Glean& operator=(Glean&&) = delete;

  /**
   * Get a Thrift client to talk to the server, with an optional shard.
   */
  std::unique_ptr<thrift::GleanServiceAsyncClient> getClient(
      folly::Optional<Shard> shard = folly::none);

  /**
   * Find the hash of the latest available DB for the given repo name.
   */
  Repo getLatestRepo(std::string repoName);

  /**
   * Return the shard number for a Repo.
   */
  static Shard getShard(Repo repo);

  /**
   * Derive a predicate name from the name of a Thrift-generated
   * schema type. For example,
   *   "facebook::glean::schema::cxx1::FunctionDeclaration"
   * would return
   *   "cxx1.FunctionDeclaration".
   */
  static std::string predicateFromType(
      const std::string& ty // use boost::typeindex::type_id<T>().pretty_name()
  );

 private:
  std::shared_ptr<facebook::config::IConfigeratorConfig<ClientConfig>> config_;
  facebook::servicerouter::cpp2::ClientFactory& factory_;
};


/**
 * A Glean client for a specific repo/database.
 */
class GleanRepo {
 public:
  using Repo = thrift::Repo;
  using Version = thrift::Version;

  /**
   * Constructors that specify a Repo
   */
  explicit GleanRepo(Repo repo);
  explicit GleanRepo(Repo repo, ClientConfig config);
  explicit GleanRepo(Repo repo, std::shared_ptr<Glean> glean);

  /**
   * Constructors that find the latest available Repo of a given name.
   */
  explicit GleanRepo(std::string repoName);
  explicit GleanRepo(std::string repoName, ClientConfig config);
  explicit GleanRepo(std::string repoName, std::shared_ptr<Glean> glean);

  GleanRepo(const GleanRepo&) = delete;
  GleanRepo(GleanRepo&&) = delete;
  GleanRepo& operator=(const GleanRepo&) = delete;
  GleanRepo& operator=(GleanRepo&&) = delete;

  Repo getRepo() { return repo_; }

  /**
   * Get a Thrift client for talking to a server directly, using a
   * shard number derived from the Repo to ensure that we talk to a
   * server that has the required DB.  Note: you should only use this
   * client for making requests against the same Repo that this
   * GleanRepo was created with.
   */
  std::unique_ptr<thrift::GleanServiceAsyncClient> getClient() {
    return glean_->getClient(shard_);
  }

  /**
   * Perform a query using JSON syntax, returning results as JSON.
   * This also allows the UserQueryOptions to be overridden, which is
   * useful for example if you want to set 'recursive', or
   * `max_results'.
   */
  std::vector<std::string> queryJson(
      const std::string& predicate,
      const std::string& query,
      thrift::UserQueryOptions opts = thrift::UserQueryOptions{});

  /**
   * Like queryJson, but specifying a particular version of the
   * predicate to query. When the version is omitted, we get the
   * latest version of the predicate.
   */
  std::vector<std::string> queryJson(
      const std::string& predicate,
      Version predicateVersion,
      const std::string& query,
      thrift::UserQueryOptions opts = thrift::UserQueryOptions{});

  /**
   * Make a query using JSON syntax, returning results as Thrift
   * types. This also allows the UserQueryOptions to be overridden,
   * which is useful for example if you want to set 'recursive', or
   * `max_results'.
   */
  template<typename T>
  std::vector<T> query(
      const std::string& query,
      thrift::UserQueryOptions opts = thrift::UserQueryOptions{}) {
    auto predicate = Glean::predicateFromType(
        boost::typeindex::type_id<T>().pretty_name());
    auto jresults = queryJson(predicate, query, opts);
    std::vector<T> results;
    results.reserve(jresults.size());
    for (auto& res : jresults) {
      results.push_back(
          apache::thrift::SimpleJSONSerializer::deserialize<T>(res));
    }
    return results;
  }

 private:
  std::shared_ptr<Glean> glean_;
  Repo repo_;
  Glean::Shard shard_;
  thrift::UserQueryClientInfo clientInfo_;
};

} // namespace glean
} // namespace facebook
