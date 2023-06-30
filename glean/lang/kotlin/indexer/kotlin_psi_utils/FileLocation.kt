/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

import com.intellij.psi.PsiElement
import glean.lang.kotlin.predicates.DeclarationLocationPredicate

fun buildFileLocation(element: PsiElement): DeclarationLocationPredicate? {
  return if (element.containingFile.viewProvider.document == null) {
    null
  } else DeclarationLocationPredicate(buildFile(element.containingFile), buildSpan(element))
}

class EmptyDeclarationLocation(name: String, elementType: String) :
    Exception(
        "Declaration location for element $name of type $elementType is required to be non empty")
