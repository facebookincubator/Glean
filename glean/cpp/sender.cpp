/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/cpp/sender.h"
#include "glean/if/gen-cpp2/GleanServiceAsyncClient.h"

#include <folly/FileUtil.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/futures/Retrying.h>
#include <thrift/lib/cpp2/protocol/Serializer.h>

#if FACEBOOK
#include "glean/facebook/cpp/service.h"
#endif

namespace facebook {
namespace glean {

using namespace facebook::glean::cpp;

namespace {

class ThriftSender : public Sender {
public:
  struct Config {
    std::string repo_name;
    std::string repo_hash;
    double min_retry_delay;
    size_t max_errors;
  };

  ThriftSender(
    std::unique_ptr<thrift::GleanServiceAsyncClient> cli,
    const Config& cfg)
    : client(std::move(cli)), config(cfg) {}

  void rebaseAndSend(BatchBase& batch, bool wait = false) override {
    if (future && (wait || future->isReady())) {
      // We've already sent a batch and received back a substitution.
      batch.rebase(std::move(*future).get());
      future.reset();
    }

    if (!future) {
      // We aren't waiting on a substitution (either we haven't sent anything
      // yet or we just rebased). Send the next piece.
      thrift::Repo repo;
      repo.name_ref() = config.repo_name;
      repo.hash_ref() = config.repo_hash;
      thrift::ComputedBatch cbatch;
      cbatch.repo_ref() = std::move(repo);
      cbatch.remember_ref() = true;
      cbatch.batch_ref() = batch.serialize();
      future = std::make_unique<folly::Future<thrift::Subst>>(
        send(
          std::make_shared<thrift::ComputedBatch>(std::move(cbatch)))
        .via(folly::getIOExecutor().get()));
    }
  }

  void flush(BatchBase& batch) override {
    rebaseAndSend(batch, true);
    if (future) {
      future->wait();
      future.reset();
    }
  }

private:
  // Communicate with the server, retrying if necessary.
  template<typename F>
  folly::invoke_result_t<F, thrift::GleanServiceAsyncClient *>
      communicate(F f) const {
    // I *think* ...Backoff mutates its closure so we need to create a new one
    // for each communication request.
    auto backoff =
      // hardcode all the things for now
      folly::futures::retryingPolicyCappedJitteredExponentialBackoff(
        config.max_errors,
        std::chrono::seconds(1),    // minimum wait
        std::chrono::seconds(30),   // maximum wait
        0.2);                       // jitter
    return folly::futures::retryingUnsafe(
      // We can't use backoff directly because we only want to retry on
      // TTransportException, not on any other exception.
      [backoff = std::move(backoff)]
          (size_t n, const folly::exception_wrapper& ew) {
        if (auto *e = ew.get_exception<
                          apache::thrift::transport::TTransportException>()) {
          LOG(ERROR) << "communication error (" << n << "): " << e->what();
          return backoff(n, ew);
        } else {
          return folly::makeFuture(false);
        }
      },
      [f=std::move(f), client=client.get()](size_t) { return f(client); }
    );
  }

  // Retry a communication request after a delay.
  template<typename F>
  auto retry(thrift::BatchRetry&& retry, F f) const {
    const auto duration =
      std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::duration<double>(
          std::max(retry.get_seconds(), config.min_retry_delay)));
    return folly::futures::sleep(duration).deferValue(
        [f = std::move(f)](auto&&) { return std::move(f)(); });
  }

  [[noreturn]] static void abort(const char *what) {
    LOG(FATAL) << what;
  }

  // Send the batch and then wait for the substitution
  folly::Future<thrift::Subst> send(
      const std::shared_ptr<thrift::ComputedBatch>& batch) const {
    return communicate([batch](auto client) {
             return client->future_sendBatch(*batch);
           })
        .thenValue([batch, this](thrift::SendResponse&& response) {
          switch (response.getType()) {
            case thrift::SendResponse::handle:
              // Server accepted the batch, now wait
              return finish(response.get_handle(), batch).semi();

            case thrift::SendResponse::retry:
              // Server asked to retry after a delay
              return retry(
                  response.move_retry(), [batch, this] { return send(batch); });

            default:
              abort("invalid SendResponse");
          }
        })
        .thenError([](const folly::exception_wrapper& error) -> folly::Future<thrift::Subst> {
          LOG(FATAL) << "unexpected error: " << error.what();
        });
  }

  // Wait for the substitution for the given handle. Resend the batch if the
  // server forgot the handle.
  folly::Future<thrift::Subst> finish(
      const std::string& handle,
      const std::shared_ptr<thrift::ComputedBatch>& batch) const {
    return communicate([handle](auto client) {
             return client->future_finishBatch(handle);
           })
        .thenValue([handle, batch, this](thrift::FinishResponse&& response) {
          switch (response.getType()) {
            case thrift::FinishResponse::subst:
              // We're done
              return folly::makeSemiFuture(response.move_subst());

            case thrift::FinishResponse::retry:
              // Server asked to retry after a delay
              return retry(response.move_retry(), [handle, batch, this] {
                return finish(handle, batch);
              });

            default:
              abort("invalid FinishResponse");
          }
        })
        .thenError([handle, batch, this](const folly::exception_wrapper& error) -> folly::SemiFuture<thrift::Subst> {
          if (error.is_compatible_with<facebook::glean::thrift::UnknownBatchHandle>()) {
            // Server forgot the handle, resend the batch
            LOG(ERROR) << "server reports unknown handle " << handle;
            return this->send(batch).semi();
          }
          LOG(FATAL) << "unexpected error:" << error.what();
        });
  }

  const std::unique_ptr<thrift::GleanServiceAsyncClient> client;
  const Config config;
  std::unique_ptr<folly::Future<thrift::Subst>> future;
};

}

std::unique_ptr<Sender> thriftSender(
    const std::string& service,
    const std::string& repo_name,
    const std::string& repo_hash,
    double min_retry_delay,
    size_t max_errors) {
  return std::make_unique<ThriftSender>(
    cpp::service(service),
    ThriftSender::Config{
      repo_name, repo_hash, min_retry_delay, max_errors
    }
  );
}

namespace {

class FileWriter : public Sender {
public:
  explicit FileWriter(std::string p) : path(std::move(p)) {}

  void rebaseAndSend(BatchBase&, bool) override {
    // don't do anything
    // NOTE: we ignore the 'wait' flag for now
  }

  void flush(BatchBase& batch) override {
    auto r = batch.serialize();
    folly::writeFile(
      apache::thrift::CompactSerializer::serialize<std::string>(r),
      path.c_str());
  }

private:
  std::string path;
};

}

std::unique_ptr<Sender> fileWriter(std::string path) {
  return std::make_unique<FileWriter>(std::move(path));
}


}
}
