/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

import glean.lang.kotlin.predicates.VariableDeclarationPredicate
import org.jetbrains.kotlin.backend.jvm.ir.psiElement
import org.jetbrains.kotlin.descriptors.ParameterDescriptor
import org.jetbrains.kotlin.resolve.BindingContext

fun buildVariableDeclaration(
    parameter: ParameterDescriptor,
    bindingContext: BindingContext
): VariableDeclarationPredicate {
  return VariableDeclarationPredicate(
      parameter.qualifiedName(),
      buildKotlinType(parameter.type, bindingContext),
      parameter.psiElement?.let { buildFileLocation(it) }
          ?: throw EmptyDeclarationLocation(parameter.name.identifier, "variable"))
}
