/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.kotlin_alpha.ClassDeclaration as GleanClassDeclaration
import com.facebook.glean.schema.kotlin_alpha.ClassDeclarationKey as GleanClassDeclarationKey

class ClassDeclarationPredicate(
    private val name: QNamePredicate,
    private val implements: List<QNamePredicate>,
    private val location: DeclarationLocationPredicate
) : GleanPredicate<GleanClassDeclaration> {
  override fun toGleanType(): GleanClassDeclaration {
    return GleanClassDeclaration.Builder()
        .apply {
          key =
              GleanClassDeclarationKey.Builder()
                  .apply {
                    name = this@ClassDeclarationPredicate.name.toGleanType()
                    implements_ = this@ClassDeclarationPredicate.implements.map { it.toGleanType() }
                    location = this@ClassDeclarationPredicate.location.toGleanType()
                  }
                  .build()
        }
        .build()
  }
}
