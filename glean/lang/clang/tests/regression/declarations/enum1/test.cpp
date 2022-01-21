/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct U {
  enum InU { UA, UB, UC };
  enum class InUClass { UCA, UCB, UCC };
};

enum Global { GA, GB, GC };
enum class GlobalClass { GCA, GCB, GCC };
