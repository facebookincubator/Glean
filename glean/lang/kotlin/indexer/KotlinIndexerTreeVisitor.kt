/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

import glean.lang.kotlin.indexer.glean_utils.buildMethodDeclaration
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.psi.KtTreeVisitorVoid
import org.jetbrains.kotlin.resolve.BindingContext

/** Kotlin Indexer compiler plugin tree visitor */
class KotlinIndexerTreeVisitor(
    private val file: KtFile,
    private val messageCollector: MessageCollector,
    private val bindingContext: BindingContext,
    private val indexContext: KotlinIndexContext
) : KtTreeVisitorVoid() {

  /** Do a round of visit for file, then summarize and build index */
  fun buildResult() {
    file.accept(this)
  }

  override fun hashCode(): Int {
    return super.hashCode()
  }

  override fun visitNamedFunction(function: KtNamedFunction) {
    indexContext.addMethods(buildMethodDeclaration(function, bindingContext))
    super.visitNamedFunction(function)
  }
}
