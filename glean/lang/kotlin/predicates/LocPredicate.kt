/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.src.Loc as GleanSrcLoc

class LocPredicate(
    private val file: FilePredicate,
    private val line: Long,
    private val column: Long
) : GleanPredicate<GleanSrcLoc> {
  override fun toGleanType(): GleanSrcLoc {
    return GleanSrcLoc.Builder()
        .apply {
          file = this@LocPredicate.file.toGleanType()
          line = this@LocPredicate.line
          column = this@LocPredicate.column
        }
        .build()
  }
}
