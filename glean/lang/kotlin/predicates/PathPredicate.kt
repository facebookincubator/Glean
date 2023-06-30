/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.javakotlin_alpha.Name
import com.facebook.glean.schema.javakotlin_alpha.Path as GleanPath
import com.facebook.glean.schema.javakotlin_alpha.PathKey

class PathPredicate(private val base: String, private val container: PathPredicate?) :
    GleanPredicate<GleanPath> {
  override fun toGleanType(): com.facebook.glean.schema.javakotlin_alpha.Path {
    val name = Name.Builder().setKey(base).build()
    val keyBuilder = PathKey.Builder().setBase(name)
    container?.let { keyBuilder.setContainer(it.toGleanType()) }
    return GleanPath.Builder().setKey(keyBuilder.build()).build()
  }

  fun toQName(emptyContainerHandler: () -> PathPredicate): QNamePredicate {
    return QNamePredicate(base, container ?: emptyContainerHandler())
  }

  fun amendBase(baseHandler: (String) -> String): PathPredicate {
    return PathPredicate(baseHandler(base), container)
  }
}
