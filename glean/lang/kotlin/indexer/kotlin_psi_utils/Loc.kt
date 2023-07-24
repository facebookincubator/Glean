/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import glean.lang.kotlin.predicates.FilePredicate
import glean.lang.kotlin.predicates.LocPredicate
import glean.lang.kotlin.predicates.SpanPredicate
import org.jetbrains.kotlin.diagnostics.PsiDiagnosticUtils
import org.jetbrains.kotlin.psi.psiUtil.endOffset
import org.jetbrains.kotlin.psi.psiUtil.startOffset

fun buildFile(file: PsiFile): FilePredicate {
  return FilePredicate(normalizePath(file.containingFile.virtualFile.path))
}

fun buildSpan(element: PsiElement): SpanPredicate {
  return SpanPredicate(start = element.startOffset.toLong(), end = element.endOffset.toLong())
}

// deprecated
fun buildLoc(element: PsiElement): LocPredicate? {
  if (element.containingFile.viewProvider.document == null) {
    return null
  }
  val lineAndColumn =
      PsiDiagnosticUtils.offsetToLineAndColumn(
          element.containingFile.viewProvider.document, element.startOffset)
  return LocPredicate(
      buildFile(element.containingFile),
      line = lineAndColumn.line.toLong(),
      column = lineAndColumn.column.toLong())
}
