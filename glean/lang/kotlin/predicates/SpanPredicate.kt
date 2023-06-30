/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.src.ByteSpan as GleanSrcByteSpan

class SpanPredicate(private val start: Long, private val end: Long) :
    GleanPredicate<GleanSrcByteSpan> {
  override fun toGleanType(): com.facebook.glean.schema.src.ByteSpan {
    return com.facebook.glean.schema.src.ByteSpan.Builder()
        .setStart(start)
        .setLength(end - start + 1)
        .build()
  }
}
