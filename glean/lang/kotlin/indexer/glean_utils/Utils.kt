/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

private const val FBSOURCE = "fbsource"

fun normalizePath(path: String): String {
  // Paths may contain fbsource more than once, fbsource-, or, number dash directory
  var fbSourceStartIndex: Int = path.indexOf(FBSOURCE)
  var pathWithoutFbSource = path
  while (fbSourceStartIndex != -1) {
    pathWithoutFbSource = pathWithoutFbSource.substring(fbSourceStartIndex + FBSOURCE.length)
    fbSourceStartIndex = pathWithoutFbSource.indexOf(FBSOURCE)
  }
  val splitPath = pathWithoutFbSource.split("[0-9]*-[0-9]*".toRegex()).toTypedArray()
  val cleanPath =
      if (splitPath.size == 1) splitPath[0]
      else if (splitPath.isEmpty()) path else java.lang.String.join("", *splitPath)
  return if (cleanPath.startsWith(FBSOURCE)) cleanPath
  else FBSOURCE + if (cleanPath.startsWith("//")) cleanPath.substring(1) else cleanPath
}
