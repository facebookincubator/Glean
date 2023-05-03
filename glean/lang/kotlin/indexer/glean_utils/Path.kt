/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.javakotlin_alpha.Path
import com.facebook.glean.schema.javakotlin_alpha.PathKey

fun buildPath(pathSegments: List<org.jetbrains.kotlin.name.Name>): Path? {
  if (pathSegments.isEmpty()) {
    return null
  }
  var keyBuilder = PathKey.Builder().setBase(buildName(pathSegments.last().identifier))

  if (pathSegments.size > 1) {
    val subPath = buildPath(pathSegments.subList(0, pathSegments.size - 2))
    keyBuilder = keyBuilder.setContainer(subPath)
  }
  return Path.Builder().setKey(keyBuilder.build()).build()
}
