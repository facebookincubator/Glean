/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/thrift.thrift"

enum fb303_status {
  DEAD = 0,
  STARTING = 1,
  ALIVE = 2,
  STOPPING = 3,
  STOPPED = 4,
  WARNING = 5,
}

service BaseService {
  @thrift.Priority{level = thrift.RpcPriority.IMPORTANT}
  fb303_status getStatus();
  string getName();
  @thrift.Priority{level = thrift.RpcPriority.IMPORTANT}
  i64 aliveSince();
}
