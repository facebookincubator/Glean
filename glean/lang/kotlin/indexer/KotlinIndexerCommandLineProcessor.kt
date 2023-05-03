/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

import org.jetbrains.kotlin.compiler.plugin.AbstractCliOption
import org.jetbrains.kotlin.compiler.plugin.CliOption
import org.jetbrains.kotlin.compiler.plugin.CliOptionProcessingException
import org.jetbrains.kotlin.compiler.plugin.CommandLineProcessor
import org.jetbrains.kotlin.config.CompilerConfiguration

/**
 * Kotlin Indexer Plugin's command line processor. Handles plugin options.
 *
 * Note that this needs a resource file
 * META-INF/services/org.jetbrains.kotlin.compiler.plugin.CommandLineProcessor for the compiler to
 * find it and load it
 */
@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
class KotlinIndexerCommandLineProcessor : CommandLineProcessor {
  companion object {
    const val KOTLIN_INDEXER_PLUGIN_ID: String = "glean.lang.kotlin.indexer"
    val OUTPUT_DIR_OPTION: CliOption =
        CliOption("outputDir", "<directory>", "Directory to output index.")
  }

  override val pluginId: String = KOTLIN_INDEXER_PLUGIN_ID
  override val pluginOptions: Collection<CliOption> = listOf(OUTPUT_DIR_OPTION)

  override fun processOption(
      option: AbstractCliOption,
      value: String,
      configuration: CompilerConfiguration
  ) {
    when (option) {
      OUTPUT_DIR_OPTION -> configuration.put(KotlinIndexerConfigurationKeys.OUTPUT_DIR_KEY, value)
      else -> throw CliOptionProcessingException("Unknown option: ${option.optionName}")
    }
  }
}
