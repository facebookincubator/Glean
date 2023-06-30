/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.kotlin_alpha.Type as GleanKotlinType
import com.facebook.glean.schema.kotlin_alpha.TypeKey as GleanKotlinTypeKey

class KotlinTypePredicate(
    private val type: JavaKotlinTypePredicate,
    private val location: DeclarationLocationPredicate?,
    private val typeArgs: List<TypeArgPredicate>,
    private val isNullable: Boolean
) : GleanPredicate<GleanKotlinType> {
  override fun toGleanType(): com.facebook.glean.schema.kotlin_alpha.Type {
    val typeKey =
        GleanKotlinTypeKey.Builder()
            .apply {
              type = this@KotlinTypePredicate.type.toGleanType()
              location = this@KotlinTypePredicate.location?.toGleanType()
              typeArgs = this@KotlinTypePredicate.typeArgs.map { it.toGleanType() }
              isIsNullable = isNullable
            }
            .build()
    return GleanKotlinType.Builder().setKey(typeKey).build()
  }
}
