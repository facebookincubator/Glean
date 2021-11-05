// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/cpp/glean.h"

namespace facebook {
namespace glean {

struct Sender {
  virtual ~Sender() {}

  // If we called rebaseAndSend previously, check if we have a response from
  // the server (or wait for it if 'wait' is true) and rebase the batch (i.e.,
  // adjust fact ids based on the substitution received from the server and
  // move all facts that the server now has from the write buffer to the cache).
  // After rebasing or if this is the first call to the function, start sending
  // all facts that are currently in the write buffer to the server.
  //
  // This retries as necessary on exceptions and aborts the program on
  // unrecoverable errors.
  //
  // The intended use pattern is this:
  //
  // while (...) {
  //    produce some facts
  //    sender->rebaseAndSend
  // }
  // sender->flush
  //
  virtual void rebaseAndSend(cpp::BatchBase& batch, bool wait = false) = 0;

  // Send all remaining batch data as soon as possible and wait for commit
  // acknowledgement.
  virtual void flush(cpp::BatchBase& batch) = 0;
};

// A Sender which sends data synchronously via Thrift.
std::unique_ptr<Sender> thriftSender(
  const std::string& service,
  const std::string& repo_name,
  const std::string& repo_hash,
  double min_retry_delay,
  size_t max_errors
);

// A Sender which dumps all data into a file. This happens on the final flush,
// rebaseAndSend is a noop. This is mostly useful for testing.
std::unique_ptr<Sender> fileWriter(
  std::string path
);

}
}
