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

fun buildFileLocation(element: PsiElement): DeclarationLocation {
  val key = DeclarationLocationKey.Builder()
  key.file = buildFile(element.containingFile)
  key.span = buildSpan(element)
  return DeclarationLocation.Builder().setKey(key.build()).build()
}
