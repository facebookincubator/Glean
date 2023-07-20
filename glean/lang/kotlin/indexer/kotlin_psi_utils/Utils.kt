/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

private val FBSOURCE = "^/.*[/-]fbsource/".toRegex()

private val NOISE = "^[0-9]+-[0-9]+/".toRegex()

// Reimplementation of java-alpha indexer Utils.java path normalizer
fun normalizePath(path: String): String {
  val cleanPath = FBSOURCE.replaceFirst(path, "")
  return NOISE.replaceFirst(cleanPath, "")
}
