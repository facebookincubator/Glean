/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.javakotlin_alpha.Name as GleanName

class NamePredicate(val name: String) : GleanPredicate<GleanName> {
  override fun toGleanType(): GleanName {
    return GleanName.Builder().setKey(name).build()
  }
}
