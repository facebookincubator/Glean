/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

import com.intellij.psi.PsiElement
import glean.lang.kotlin.predicates.MethodDeclarationPredicate
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.bindingContextUtil.getAbbreviatedTypeOrType

class MethodDeclarationBuilderError(message: String, functionName: String) :
    Throwable("Failed to build method declaration for $functionName: $message") {}

private fun getFunctionDescriptor(
    namedFunction: KtNamedFunction,
    bindingContext: BindingContext
): FunctionDescriptor? {
  return bindingContext[BindingContext.FUNCTION, namedFunction]
}

fun buildMethodDeclaration(
    function: KtNamedFunction,
    bindingContext: BindingContext
): MethodDeclarationPredicate {
  val funcDescriptor =
      getFunctionDescriptor(function, bindingContext)
          ?: throw MethodDeclarationBuilderError(
              "Could not get function descriptor", function.name ?: "")
  val kotlinType = function.typeReference?.getAbbreviatedTypeOrType(bindingContext)
  val ktClassBody = function.parent
  return MethodDeclarationPredicate(
      buildMethodName(funcDescriptor),
      funcDescriptor.valueParameters.map { parameter ->
        buildVariableDeclaration(parameter, bindingContext)
      },
      kotlinType?.let { buildKotlinType(it, bindingContext) },
      (ktClassBody?.parent as? KtClass)?.let { buildClassDeclaration(it, bindingContext) },
      (function as? PsiElement)?.let { buildLoc(it) }
          ?: throw MissingRequiredGleanFieldException("MethodDeclaration.loc"),
      (function as? PsiElement)?.let { buildFileLocation(it) }
          ?: throw MissingRequiredGleanFieldException("MethodDeclaration.location"))
}
