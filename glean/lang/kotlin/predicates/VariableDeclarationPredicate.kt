/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.kotlin_alpha.VariableDeclaration as GleanVariableDeclaration
import com.facebook.glean.schema.kotlin_alpha.VariableDeclarationKey as GleanVariableDeclarationKey

class VariableDeclarationPredicate(
    private val name: QNamePredicate,
    private val type: KotlinTypePredicate,
    private val location: DeclarationLocationPredicate
) : GleanPredicate<GleanVariableDeclaration> {
  override fun toGleanType(): GleanVariableDeclaration {
    return GleanVariableDeclaration.Builder()
        .apply {
          key =
              GleanVariableDeclarationKey.Builder()
                  .apply {
                    name = this@VariableDeclarationPredicate.name.toGleanType()
                    type = this@VariableDeclarationPredicate.type.toGleanType()
                    location = this@VariableDeclarationPredicate.location.toGleanType()
                  }
                  .build()
        }
        .build()
  }
}
