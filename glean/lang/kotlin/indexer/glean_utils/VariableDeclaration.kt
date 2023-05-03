/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.kotlin_alpha.VariableDeclaration
import com.facebook.glean.schema.kotlin_alpha.VariableDeclarationKey
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.resolve.BindingContext

fun buildVariableDeclaration(
    parameter: KtParameter,
    bindingContext: BindingContext
): VariableDeclaration? {
  val ownerFunction = parameter.ownerFunction as? KtNamedFunction
  val fqName = ownerFunction?.fqName ?: return null
  val name = ownerFunction.name ?: return null
  val typeReference = ownerFunction.typeReference ?: return null

  val keyBuilder = VariableDeclarationKey.Builder()
  keyBuilder.name = buildQName(name, fqName)
  keyBuilder.type = buildType(typeReference, bindingContext)
  keyBuilder.location = buildFileLocation(parameter.originalElement)
  return VariableDeclaration.Builder().setKey(keyBuilder.build()).build()
}
