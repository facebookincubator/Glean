/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.src.ByteSpan
import com.facebook.glean.schema.src.File
import com.facebook.glean.schema.src.Loc
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import glean.lang.kotlin.indexer.normalizePath
import org.jetbrains.kotlin.diagnostics.PsiDiagnosticUtils
import org.jetbrains.kotlin.psi.psiUtil.endOffset
import org.jetbrains.kotlin.psi.psiUtil.startOffset

fun buildFile(file: PsiFile): File {
  return File.Builder().setKey(normalizePath(file.containingFile.virtualFile.path)).build()
}

fun buildSpan(element: PsiElement): ByteSpan {
  return ByteSpan.Builder()
      .setStart(element.startOffset.toLong())
      .setLength((element.endOffset - element.startOffset + 1).toLong())
      .build()
}

fun buildLoc(element: PsiElement): Loc? {
  if (element.containingFile.viewProvider.document == null) {
    return null
  }
  val lineAndColumn =
      PsiDiagnosticUtils.offsetToLineAndColumn(
          element.containingFile.viewProvider.document, element.startOffset)
  return Loc.Builder()
      .setFile(buildFile(element.containingFile))
      .setLine(lineAndColumn.line.toLong())
      .setColumn(lineAndColumn.column.toLong())
      .build()
}
