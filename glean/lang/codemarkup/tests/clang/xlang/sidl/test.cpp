/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "test.h"
#include "test_types.h"

namespace sidl = glean::sidl;

// No IDL entity by design.
struct Plain {
  int version;
};

// Calls through this class have no IDL entity by design.
class EchoServiceImpl : public sidl::IEchoService {
 public:
  ::arfx::osf::FutureStatusOr<int32_t> echoAsync(int32_t value) noexcept override;
};

int optionsRetries(const sidl::Options& options) {
  return options.retries;
}

// Uses every annotated declaration, so xrefs here cover all kinds.
int useAll(
    sidl::IEchoService& service,
    EchoServiceImpl& impl,
    const sidl::Request& request,
    const Plain& plain) {
  sidl::Options options = request.options;
  sidl::Millis timeout = options.timeout;
  sidl::Mode mode =
      request.mode == sidl::Mode::On ? sidl::Mode::On : sidl::Mode::Off;
  service.echoAsync(sidl::kLimit);
  service.configureAsync(request);
  impl.echoAsync(plain.version);
  return static_cast<int>(timeout) + static_cast<int>(mode) +
      optionsRetries(options);
}
