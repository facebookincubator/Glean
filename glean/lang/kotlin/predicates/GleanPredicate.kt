/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.thrift.payload.ThriftSerializable

interface GleanPredicate<T> where T : ThriftSerializable {
  fun toGleanType(): T
}
