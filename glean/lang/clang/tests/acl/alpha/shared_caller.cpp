/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "../public/shared.h"
#include "tented_shared.h"

int acl_alpha_shared_caller() {
  return acl_shared_function(2) + acl_tented_shared_header_function();
}
