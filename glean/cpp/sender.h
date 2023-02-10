/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

}
}
