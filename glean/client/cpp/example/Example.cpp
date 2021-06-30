// Copyright (c) Facebook, Inc. and its affiliates.

#include "glean/client/cpp/GleanClient.h"
#include "common/init/Init.h"
#include "common/network/NetworkUtil.h"
#include "glean/schema/thrift/gen-cpp2/cxx1_types.h"

using namespace apache::thrift;
using namespace facebook;
using namespace facebook::glean;
using namespace facebook::glean::schema;
using namespace facebook::network;

DEFINE_string(host, "", "hostname:port");
DEFINE_string(tier, "", "tiername");

int main(int argc, char* argv[]) {
  initFacebook(&argc, &argv);
  std::unique_ptr<GleanRepo> glean;

  if (!FLAGS_tier.empty()) {
    ClientConfig config;
    Service serv;
    serv.tier_ref() = FLAGS_tier;
    config.serv_ref() = std::move(serv);
    glean = std::make_unique<GleanRepo>("fbsource", config);
  } else if (!FLAGS_host.empty()) {
    std::string host, port;
    if (!NetworkUtil::splitHostPort(FLAGS_host, &host, &port)) {
      LOG(ERROR) << "malformed hostname:port: " << FLAGS_host;
      exit(1);
    }
    HostPort hostport;
    Service serv;
    ClientConfig config;
    hostport.host_ref() = host;
    hostport.port_ref() = folly::to<int32_t>(port);
    serv.hostPort_ref() = std::move(hostport);
    config.serv_ref() = std::move(serv);
    glean = std::make_unique<GleanRepo>("fbsource", config);
  } else {
    glean = std::make_unique<GleanRepo>("fbsource");
  }

  auto opts = facebook::glean::thrift::UserQueryOptions{};
  opts.max_results_ref() = 10;
  opts.recursive_ref() = true;

  auto query =
    "{ \"key\": { \"name\": {\"key\": {\"name\": {\"key\": \"Future\"}}}}}";

  auto results = glean->queryJson(
      "cxx1.RecordDeclaration",
      query,
      opts);
  for (auto& res : results) {
    std::cout << res;
  }

  auto results2 = glean->query<cxx1::RecordDeclaration>(query, opts);
  for (auto& res : results2) {
    std::cout << SimpleJSONSerializer::serialize<std::string>(res) << "\n";
  }
}
