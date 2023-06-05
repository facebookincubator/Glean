/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.kotlin_alpha.MethodDeclaration
import com.facebook.glean.schema.kotlin_alpha.MethodDeclarationKey
import com.intellij.psi.PsiElement
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
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
): MethodDeclaration {
  val keyBuilder = MethodDeclarationKey.Builder()
  (function as? PsiElement)?.let {
    keyBuilder.loc = buildLoc(it)
  }

  val funcDescriptor =
      getFunctionDescriptor(function, bindingContext)
          ?: throw MethodDeclarationBuilderError(
              "Could not get function descriptor", function.name ?: "")
  keyBuilder.parameters =
      funcDescriptor.valueParameters.map { parameter ->
        buildVariableDeclaration(parameter, bindingContext)
      }
  val kotlinType = function.typeReference?.getAbbreviatedTypeOrType(bindingContext)
  if (kotlinType != null) {
    keyBuilder.returnType = buildKotlinType(kotlinType, bindingContext)
  }
  keyBuilder.name = buildMethodName(funcDescriptor)
  return MethodDeclaration.Builder().setKey(keyBuilder.build()).build()
}
