/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

import org.jetbrains.kotlin.config.CompilerConfigurationKey

object KotlinIndexerConfigurationKeys {
  val OUTPUT_DIR_KEY: CompilerConfigurationKey<String> =
      CompilerConfigurationKey.create("output directory")
}
