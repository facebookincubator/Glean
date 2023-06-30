/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.src.File as GleanSrcFile

class FilePredicate(private val path: String) : GleanPredicate<GleanSrcFile> {
  override fun toGleanType(): GleanSrcFile {
    return GleanSrcFile.Builder().setKey(path).build()
  }
}
