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
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.resolve.BindingContext

fun buildMethodDeclaration(
    function: KtNamedFunction,
    bindingContext: BindingContext
): MethodDeclaration {
  val keyBuilder = MethodDeclarationKey.Builder()
  if (function.contractDescription != null) {
    keyBuilder.loc = buildLoc(function.contractDescription!!.psiOrParent as PsiElement)
  }
  keyBuilder.parameters =
      function.valueParameters.map { ktParameter ->
        buildVariableDeclaration(ktParameter, bindingContext)
      }
  if (function.typeReference != null) {
    keyBuilder.returnType = buildType(function.typeReference!!, bindingContext)
  }
  return MethodDeclaration.Builder().setKey(keyBuilder.build()).build()
}
