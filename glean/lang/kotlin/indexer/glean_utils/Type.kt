/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.javakotlin_alpha.Type as JavaKotlinType
import com.facebook.glean.schema.kotlin_alpha.Type
import com.facebook.glean.schema.kotlin_alpha.TypeArg
import com.facebook.glean.schema.kotlin_alpha.TypeArgKey
import com.facebook.glean.schema.kotlin_alpha.TypeKey
import org.jetbrains.kotlin.backend.jvm.ir.psiElement
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.typeUtil.TypeNullability
import org.jetbrains.kotlin.types.typeUtil.nullability

private fun KotlinType.javakotlinType(): JavaKotlinType {
  return JavaKotlinType.Builder().setKey(this.gleanType()).build()
}

private fun KotlinType.asTypeArg(bindingContext: BindingContext): TypeArg {
  val key = TypeArgKey.Builder().setType(buildKotlinType(this, bindingContext)).build()
  return TypeArg.Builder().setKey(key).build()
}

fun buildKotlinType(typeReference: KotlinType, bindingContext: BindingContext): Type {
  val typeKey = TypeKey.Builder()
  typeKey.type = typeReference.javakotlinType()
  val typeConstructorElement = typeReference.constructor.declarationDescriptor?.psiElement
  if (typeConstructorElement != null) {
    typeKey.location = buildFileLocation(typeConstructorElement)
  }

  val typeArgs: MutableList<TypeArg> = mutableListOf()
  for (typeArg in typeReference.arguments) {
    if (typeArg.isStarProjection) {
      continue
    }
    typeArgs.add(typeArg.type.asTypeArg(bindingContext))
  }
  typeKey.typeArgs = typeArgs

  typeKey.isIsNullable = typeReference.nullability() == TypeNullability.NULLABLE
  return Type.Builder().setKey(typeKey.build()).build()
}
