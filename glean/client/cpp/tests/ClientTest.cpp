// Copyright (c) Facebook, Inc. and its affiliates.

#include <gtest/gtest.h>
#include <vector>
#include <ext/stdio_filebuf.h>

#include "common/init/Init.h"
#include "common/network/NetworkUtil.h"
#include "folly/Subprocess.h"
#include "folly/experimental/TestUtil.h"
#include "glean/client/cpp/GleanClient.h"
#include "glean/schema/thrift/gen-cpp2/cxx1_types.h"

using namespace facebook;
using namespace facebook::glean;
using namespace facebook::glean::schema;
using namespace facebook::glean::thrift;
using namespace facebook::network;

DEFINE_string(server, "", "server executable");
DEFINE_string(db_schema, "", "path to schema JSON");

class ClientTest : public testing::Test {
 public:
  folly::test::TemporaryDirectory tmp_dir;
  folly::Subprocess server;
  std::shared_ptr<Glean> glean;
  Glean::Repo repo;

 private:
  void SetUp() override;
  void TearDown() override;
};

void ClientTest::TearDown() {
  server.terminate();
  server.wait();
}

void ClientTest::SetUp() {
  repo.name_ref() = "clienttest";
  repo.hash_ref() = "0";

  server = folly::Subprocess(
      {
          FLAGS_server,
          "--port",
          "25052",
          "--db-root",
          tmp_dir.path().string(),
          "--schema",
          "dir:" + FLAGS_db_schema
      },
      folly::Subprocess::Options().parentDeathSignal(SIGKILL));

  HostPort hostport;
  Service serv;
  ClientConfig config;
  hostport.host_ref() = "localhost";
  hostport.port_ref() = 25052;
  serv.hostPort_ref() = std::move(hostport);
  config.serv_ref() = std::move(serv);
  config.host_timeout_ms_ref() = 20000;

  glean = std::make_shared<Glean>(config);

  auto client = glean->getClient();

  do {
    try {
      auto status = client->sync_getStatus();
      if (status == fb303::cpp2::fb_status::ALIVE) break;
    } catch (std::exception &e) {
      LOG(INFO) << "sync_getStatus: " << e.what();
    }
    sleep(1);
  } while (1);

  KickOff ko;
  ko.repo_ref() = repo;
  KickOffFill fill;
  auto handle = "handle";
  fill.writeHandle_ref() = handle;
  ko.fill_ref() = std::move(fill);
  KickOffResponse kor;
  // sync_kickOff Timeouts have been making this test flaky
  int attempts = 2;
  bool kickOffSuccess = false;
  while (! kickOffSuccess) {
    --attempts;
    try {
      client->sync_kickOff(kor, ko);
    } catch (std::exception &e) {
      LOG(INFO) << "sync_kickOff: " << e.what();
      if (attempts > 0) {
        continue;
      } else {
        throw e;
      }
    }
    kickOffSuccess = true;
  }

  JsonFactBatch jfb;
  PredicateRef pr;
  pr.name_ref() = "cxx1.Name";
  pr.version_ref() = 1;
  *jfb.predicate_ref() = pr;
  *jfb.facts_ref() = {"{\"key\": \"abc\"}"};
  SendJsonBatch sjb;
  sjb.batches_ref() = {jfb};
  SendJsonBatchOptions sjbo;
  sjbo.no_base64_binary_ref() = true;
  sjb.options_ref() = sjbo;
  *sjb.remember_ref() = true;
  SendJsonBatchResponse sjbr;
  client->sync_sendJsonBatch(sjbr, repo, sjb);
  // wait for the write to finish
  FinishResponse fr;
  client->sync_finishBatch(fr, *sjbr.handle_ref());
  while (fr.getType() != FinishResponse::subst) {
    sleep(1);
    client->sync_finishBatch(fr, *sjbr.handle_ref());
  }

  Work work;
  work.repo_ref() = repo;
  work.handle_ref() = handle;
  WorkFinished wf;
  wf.work_ref() = work;
  Outcome outcome;
  outcome.success_ref() = Success{};
  wf.outcome_ref() = outcome;
  client->sync_workFinished(wf);
}

TEST_F(ClientTest, ClientTest) {
  auto gleanRepo = GleanRepo(repo, glean);

  // pure JSON query
  auto facts = gleanRepo.queryJson("cxx1.Name", "{\"get\": {}}");
  EXPECT_EQ(facts.size(), 1);
  EXPECT_TRUE(facts[0].find("\"abc\"") != std::string::npos);

  // With predicate version
  auto facts1 = gleanRepo.queryJson("cxx1.Name", 1, "{\"get\": {}}");
  EXPECT_EQ(facts1.size(), 1);
  EXPECT_TRUE(facts1[0].find("\"abc\"") != std::string::npos);

  // query returning Thrift types
  auto facts2 = gleanRepo.query<cxx1::Name>("{\"get\": {}}");
  EXPECT_EQ(facts2.size(), 1);
  EXPECT_TRUE(*facts2[0].get_key() == "abc");
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  facebook::initFacebook(&argc, &argv);
  return RUN_ALL_TESTS();
}
