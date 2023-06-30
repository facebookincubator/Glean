/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.javakotlin_alpha.QName as GleanQName
import com.facebook.glean.schema.javakotlin_alpha.QNameKey

class QNamePredicate(private val name: String, private val context: PathPredicate) :
    GleanPredicate<GleanQName> {
  override fun toGleanType(): com.facebook.glean.schema.javakotlin_alpha.QName {
    val key =
        QNameKey.Builder()
            .apply {
              name = NamePredicate(this@QNamePredicate.name).toGleanType()
              context = this@QNamePredicate.context.toGleanType()
            }
            .build()
    return GleanQName.Builder().setKey(key).build()
  }
}
