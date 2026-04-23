/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/acl/acl_provider.h"

#include <glog/logging.h>

#include "glean/rts/acl/json_acl_provider.h"

namespace facebook {
namespace glean {
namespace rts {
namespace acl {

std::unique_ptr<ACLProvider> createJsonACLProvider(
    const std::string& jsonPath,
    bool permissive) {
  VLOG(1) << "Creating JsonACLProvider from " << jsonPath
          << (permissive ? " (permissive mode)" : " (inherited mode)");
  return std::make_unique<JsonACLProvider>(jsonPath, permissive);
}

} // namespace acl
} // namespace rts
} // namespace glean
} // namespace facebook
