/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.javakotlin_alpha.MethodName as GleanMethodName
import com.facebook.glean.schema.javakotlin_alpha.MethodNameKey as GleanMethodNameKey

class MethodNamePredicate(
    private val name: QNamePredicate,
    private val signature: List<JavaKotlinTypePredicate>
) : GleanPredicate<GleanMethodName> {
  override fun toGleanType(): GleanMethodName {
    val key =
        GleanMethodNameKey.Builder()
            .apply {
              signature = this@MethodNamePredicate.signature.map { it.toGleanType() }
              name = this@MethodNamePredicate.name.toGleanType()
            }
            .build()
    return GleanMethodName.Builder().setKey(key).build()
  }
}
