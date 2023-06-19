/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.kotlin_alpha.DeclarationLocation
import com.facebook.glean.schema.kotlin_alpha.DeclarationLocationKey
import com.intellij.psi.PsiElement

fun buildFileLocation(element: PsiElement): DeclarationLocation? {
  if (element.containingFile.viewProvider.document == null) {
    return null
  }
  val key = DeclarationLocationKey.Builder()
  key.file = buildFile(element.containingFile)
  key.span = buildSpan(element)
  return DeclarationLocation.Builder().setKey(key.build()).build()
}

class EmptyDeclarationLocation(name: String, elementType: String) :
    Exception(
        "Declaration location for element $name of type $elementType is required to be non empty")
