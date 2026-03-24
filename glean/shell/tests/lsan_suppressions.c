/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Provides LSAN suppressions for the mysql_server_init leak that occurs
// during static initialization of MysqlClientBase. This is linked into
// Haskell test binaries so that the suppression is active during both
// test listing and test execution (unlike LSAN_OPTIONS env var which
// may not be applied during listing).
//
// See also: tools/build/sanitizers/asan.c which defines
// __lsan_default_suppressions() reading from this weak symbol.

const char* const kLSanDefaultSuppressions = "leak:mysql_server_init\n";
