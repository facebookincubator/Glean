/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.kotlin_alpha.TypeArg as GleanTypeArg
import com.facebook.glean.schema.kotlin_alpha.TypeArgKey as GleanTypeArgKey

class TypeArgPredicate(private val type: KotlinTypePredicate) : GleanPredicate<GleanTypeArg> {
  override fun toGleanType(): GleanTypeArg {
    val key = GleanTypeArgKey.Builder().setType(type.toGleanType()).build()
    return com.facebook.glean.schema.kotlin_alpha.TypeArg.Builder().setKey(key).build()
  }
}
