/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.javakotlin_alpha.*
import com.facebook.tools.jast.primitiveToShortFormat
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.buildPossiblyInnerType
import org.jetbrains.kotlin.js.descriptorUtils.getJetTypeFqName
import org.jetbrains.kotlin.js.descriptorUtils.nameIfStandardType
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.isFlexible
import org.jetbrains.kotlin.types.typeUtil.*
import org.jetbrains.kotlin.types.upperIfFlexible

fun buildMethodName(function: FunctionDescriptor): MethodName {
  val key = MethodNameKey.Builder().setName(function.qualifiedName())
  key.signature =
      function.valueParameters
          .map { p -> Type.Builder().setKey(p.type.gleanType()).build() }
          .toList()
  return MethodName.Builder().setKey(key.build()).build()
}

fun KotlinType.gleanType(): TypeKey {
  if (this.isPrimitiveNumberType() || this.isBoolean() || this.isUnit()) {
    val type = this.toString().lowercase()
    return TypeKey.fromPrimitive(
        primitiveToShortFormat(if (this.isMarkedNullable) type.substringBefore("?") else type))
  } else {
    if (this.isTypeParameter() || this.isAnyOrNullableAny()) {
      return TypeKey.fromObject(listOf("java", "lang", "Object").joinNonEmptyPath())
    }
    if (this.isArrayOfNothing()) {
      val objectType =
          Type.Builder()
              .setKey(TypeKey.fromObject(listOf("java", "lang", "Object").joinNonEmptyPath()))
              .build()
      return TypeKey.fromArray(objectType)
    }
    val potentialName = nameIfStandardType
    if (potentialName != null && potentialName.toString() == "Array") {
      val containedTypes = this.constituentTypes()
      if (containedTypes.size == 1) {
        return containedTypes.first().gleanType()
      }
    }
    if (this.isFlexible()) {
      return this.upperIfFlexible().gleanType()
    }

    val fqNameMaybeNullable = this.getJetTypeFqName(false)
    val fqName =
        if (this.isMarkedNullable) fqNameMaybeNullable.substringBefore("?") else fqNameMaybeNullable
    val possiblyInnerType =
        this.buildPossiblyInnerType() ?: return TypeKey.fromObject(fqName.path())
    return TypeKey.fromObject(possiblyInnerType.classDescriptor.path())
  }
}
