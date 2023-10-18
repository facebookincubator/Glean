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

private val RE_NOISE = "^/re_cwd/".toRegex()

// Reimplementation of java-alpha indexer Utils.java path normalizer
fun normalizePath(path: String): String {
  val m1 = FBSOURCE.replaceFirst(path, "")
  val m2 = NOISE.replaceFirst(m1, "")
  return RE_NOISE.replaceFirst(m2, "")
}
