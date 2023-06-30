/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

import glean.lang.kotlin.predicates.KotlinTypePredicate
import glean.lang.kotlin.predicates.TypeArgPredicate
import org.jetbrains.kotlin.backend.jvm.ir.psiElement
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.typeUtil.TypeNullability
import org.jetbrains.kotlin.types.typeUtil.nullability

private fun KotlinType.asTypeArg(bindingContext: BindingContext): TypeArgPredicate {
  return TypeArgPredicate(buildKotlinType(this, bindingContext))
}

fun buildKotlinType(
    typeReference: KotlinType,
    bindingContext: BindingContext
): KotlinTypePredicate {
  val typeConstructorElement = typeReference.constructor.declarationDescriptor?.psiElement
  return KotlinTypePredicate(
      typeReference.gleanType(),
      typeConstructorElement?.let { buildFileLocation(it) },
      typeReference.arguments
          .filter { !it.isStarProjection }
          .map { it.type.asTypeArg(bindingContext) }
          .toList(),
      typeReference.nullability() == TypeNullability.NULLABLE)
}
