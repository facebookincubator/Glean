/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.kotlin_alpha.DeclarationLocation as GleanDeclarationLocation
import com.facebook.glean.schema.kotlin_alpha.DeclarationLocationKey

class DeclarationLocationPredicate(
    private val file: FilePredicate,
    private val span: SpanPredicate
) : GleanPredicate<GleanDeclarationLocation> {
  override fun toGleanType(): GleanDeclarationLocation {
    val key =
        DeclarationLocationKey.Builder().apply {
          file = this@DeclarationLocationPredicate.file.toGleanType()
          span = this@DeclarationLocationPredicate.span.toGleanType()
        }

    return GleanDeclarationLocation.Builder().setKey(key.build()).build()
  }
}
