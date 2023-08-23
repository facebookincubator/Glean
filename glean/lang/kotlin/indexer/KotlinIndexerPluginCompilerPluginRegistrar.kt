/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.compiler.plugin.CompilerPluginRegistrar
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.resolve.jvm.extensions.AnalysisHandlerExtension

/**
 * Registers [KotlinIndexerPluginExtension] to be used by the compiler
 *
 * Note that this needs a resource file
 * META-INF/services/org.jetbrains.kotlin.compiler.plugin.ComponentRegistrar for the compiler to
 * find it and load it
 */
@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
class KotlinIndexerPluginCompilerPluginRegistrar : CompilerPluginRegistrar() {
  override fun ExtensionStorage.registerExtensions(configuration: CompilerConfiguration) {
    val outputDir = configuration.get(JVMConfigurationKeys.OUTPUT_DIRECTORY)
    checkNotNull(
        outputDir, { "Kotlin Indexer compiler plugin failed. 'outputDir' option is not provided" })

    val messageCollector =
        configuration.get(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, MessageCollector.NONE)
    messageCollector.report(
        CompilerMessageSeverity.LOGGING,
        "Loaded Kotlin Indexer Plugin, with outputDir = '$outputDir'.")

    // Create our plugin
    val extension = KotlinIndexerPluginExtension(outputDir.absolutePath, messageCollector)

    // Register it to a specific extension point in the compiler, the analysis part
    AnalysisHandlerExtension.registerExtension(extension)
  }

  override val supportsK2: Boolean = true
}
