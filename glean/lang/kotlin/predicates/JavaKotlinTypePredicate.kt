/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.javakotlin_alpha.Type as GleanJavaKotlinType
import com.facebook.glean.schema.javakotlin_alpha.TypeKey as GleanJavaKotlinTypeKey

sealed class JavaKotlinTypePredicate : GleanPredicate<GleanJavaKotlinType> {
  class StringValue(private val value: String) : JavaKotlinTypePredicate() {
    override fun toGleanType(): GleanJavaKotlinType {
      val key = GleanJavaKotlinTypeKey.fromPrimitive(value)
      return GleanJavaKotlinType.Builder().setKey(key).build()
    }
  }

  class PathValue(private val path: PathPredicate) : JavaKotlinTypePredicate() {
    override fun toGleanType(): GleanJavaKotlinType {
      val key = GleanJavaKotlinTypeKey.fromObject(path.toGleanType())
      return GleanJavaKotlinType.Builder().setKey(key).build()
    }
  }

  class ArrayValue(private val type: JavaKotlinTypePredicate) : JavaKotlinTypePredicate() {
    override fun toGleanType(): GleanJavaKotlinType {
      val key = GleanJavaKotlinTypeKey.fromArray(type.toGleanType())
      return GleanJavaKotlinType.Builder().setKey(key).build()
    }
  }
}
