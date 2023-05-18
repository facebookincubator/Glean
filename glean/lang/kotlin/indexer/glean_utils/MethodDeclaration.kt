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
  if (function.contractDescription != null) {
    keyBuilder.loc = buildLoc(function.contractDescription!!.psiOrParent as PsiElement)
  }

  val funcDescriptor =
      getFunctionDescriptor(function, bindingContext)
          ?: throw Error("Could not get function descriptor")
  keyBuilder.parameters =
      funcDescriptor.valueParameters.map { parameter ->
        buildVariableDeclaration(parameter, bindingContext)
      }
  val kotlinType = function.typeReference?.getAbbreviatedTypeOrType(bindingContext)
  if (kotlinType != null) {
    keyBuilder.returnType = buildType(kotlinType, bindingContext)
  }
  return MethodDeclaration.Builder().setKey(keyBuilder.build()).build()
}
