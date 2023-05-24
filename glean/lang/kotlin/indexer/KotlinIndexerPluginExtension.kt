/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

import com.intellij.openapi.project.Project
import org.jetbrains.kotlin.analyzer.AnalysisResult
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingTrace
import org.jetbrains.kotlin.resolve.jvm.extensions.AnalysisHandlerExtension

/** Kotlin Indexer compiler plugin. Used to create Kotlin glean index */
@Suppress(
    "AnalysisHandlerExtension") /* This transformer can be disabled and the code will still build; codemods/indexers can do that. */
class KotlinIndexerPluginExtension(
    private val outputDir: String,
    private val messageCollector: MessageCollector
) : AnalysisHandlerExtension {

  override fun analysisCompleted(
      project: Project,
      module: ModuleDescriptor,
      bindingTrace: BindingTrace,
      files: Collection<KtFile>
  ): AnalysisResult? {
    // Fail fast: This lets us assume that there is a file farther down
    if (files.isEmpty()) {
      return null
    }

    val context = KotlinIndexContext()
    for (file in files) {

      // Replace text and create new file
      val visitor =
          KotlinIndexerTreeVisitor(file, messageCollector, bindingTrace.bindingContext, context)
      visitor.buildResult()
    }

    // Not going to generate the metadata file if there are any errors
    if (messageCollector.hasErrors()) {
      messageCollector.report(
          CompilerMessageSeverity.EXCEPTION,
          "Kotlin Indexer Plugin finished but failed with one or more error.")
      AnalysisResult.compilationError(bindingTrace.bindingContext).throwIfError()
    }

    context.dump("$outputDir/glean_index.txt")
    return null
  }
}
