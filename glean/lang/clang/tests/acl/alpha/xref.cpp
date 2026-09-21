/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

int acl_public_target();
int acl_bravo_target();

int acl_alpha_marker() {
  return acl_public_target() + acl_bravo_target();
}
