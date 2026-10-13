/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "../alpha/tented.h"
#include "../alpha/tented_shared.h"

int acl_public_includes_tented() {
  return acl_tented_header_function() + acl_tented_shared_header_function();
}
