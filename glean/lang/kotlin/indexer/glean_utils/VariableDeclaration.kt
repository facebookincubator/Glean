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
import org.jetbrains.kotlin.backend.jvm.ir.psiElement
import org.jetbrains.kotlin.descriptors.ParameterDescriptor
import org.jetbrains.kotlin.resolve.BindingContext

fun buildVariableDeclaration(
    parameter: ParameterDescriptor,
    bindingContext: BindingContext
): VariableDeclaration? {
  val keyBuilder = VariableDeclarationKey.Builder()
  keyBuilder.name = parameter.qualifiedName()
  keyBuilder.type = buildKotlinType(parameter.type, bindingContext)
  val psiElement = parameter.psiElement
  if (psiElement !== null) {
    keyBuilder.location = buildFileLocation(psiElement)
  }
  return VariableDeclaration.Builder().setKey(keyBuilder.build()).build()
}
