/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.predicates

import com.facebook.glean.schema.kotlin_alpha.Declaration
import com.facebook.glean.schema.kotlin_alpha.MethodDeclaration as GleanMethodDeclaration
import com.facebook.glean.schema.kotlin_alpha.MethodDeclarationKey as GleanMethodDeclarationKey

class MethodDeclarationPredicate(
    private val name: MethodNamePredicate,
    private val parameters: List<VariableDeclarationPredicate>,
    private val returnType: KotlinTypePredicate?,
    private val classDeclaration: ClassDeclarationPredicate?,
    private val loc: LocPredicate
) : GleanPredicate<GleanMethodDeclaration> {
  override fun toGleanType(): GleanMethodDeclaration {
    return GleanMethodDeclaration.Builder()
        .apply {
          key =
              GleanMethodDeclarationKey.Builder()
                  .apply {
                    name = this@MethodDeclarationPredicate.name.toGleanType()
                    parameters = this@MethodDeclarationPredicate.parameters.map { it.toGleanType() }
                    returnType = this@MethodDeclarationPredicate.returnType?.toGleanType()
                    container =
                        this@MethodDeclarationPredicate.classDeclaration?.let {
                          Declaration.fromClass_(it.toGleanType())
                        }
                    loc = this@MethodDeclarationPredicate.loc.toGleanType()
                  }
                  .build()
        }
        .build()
  }
}
