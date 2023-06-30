/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

import com.facebook.tools.jast.primitiveToShortFormat
import glean.lang.kotlin.predicates.JavaKotlinTypePredicate
import glean.lang.kotlin.predicates.MethodNamePredicate
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.annotations.Annotations
import org.jetbrains.kotlin.descriptors.buildPossiblyInnerType
import org.jetbrains.kotlin.js.descriptorUtils.getJetTypeFqName
import org.jetbrains.kotlin.js.descriptorUtils.nameIfStandardType
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.isFlexible
import org.jetbrains.kotlin.types.typeUtil.constituentTypes
import org.jetbrains.kotlin.types.typeUtil.isAnyOrNullableAny
import org.jetbrains.kotlin.types.typeUtil.isArrayOfNothing
import org.jetbrains.kotlin.types.typeUtil.isBoolean
import org.jetbrains.kotlin.types.typeUtil.isPrimitiveNumberType
import org.jetbrains.kotlin.types.typeUtil.isTypeParameter
import org.jetbrains.kotlin.types.typeUtil.isUnit
import org.jetbrains.kotlin.types.typeUtil.replaceAnnotations
import org.jetbrains.kotlin.types.upperIfFlexible

fun buildMethodName(function: FunctionDescriptor): MethodNamePredicate {
  return MethodNamePredicate(
      function.qualifiedName(), function.valueParameters.map { it.type.gleanType() }.toList())
}

fun KotlinType.gleanType(): JavaKotlinTypePredicate {
  // we do not include annotations into names
  if (!this.annotations.isEmpty()) {
    return this.replaceAnnotations(Annotations.EMPTY).gleanType()
  }

  if (this.isPrimitiveNumberType() || this.isBoolean() || this.isUnit()) {
    val type = this.toString().lowercase()
    return JavaKotlinTypePredicate.StringValue(
        primitiveToShortFormat(if (this.isMarkedNullable) type.substringBefore("?") else type))
  } else {
    if (this.isTypeParameter() || this.isAnyOrNullableAny()) {
      return JavaKotlinTypePredicate.PathValue(listOf("java", "lang", "Object").joinNonEmptyPath())
    }
    if (this.isArrayOfNothing()) {
      return JavaKotlinTypePredicate.ArrayValue(
          JavaKotlinTypePredicate.PathValue(listOf("java", "lang", "Object").joinNonEmptyPath()))
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
        this.buildPossiblyInnerType() ?: return JavaKotlinTypePredicate.PathValue(fqName.path())
    return JavaKotlinTypePredicate.PathValue(possiblyInnerType.classDescriptor.path())
  }
}
