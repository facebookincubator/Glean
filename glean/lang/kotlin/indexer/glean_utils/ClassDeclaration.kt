/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.kotlin_alpha.ClassDeclaration as GleanClassDeclaration
import com.facebook.glean.schema.kotlin_alpha.ClassDeclarationKey as GleanClassDeclarationKey
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.bindingContextUtil.getAbbreviatedTypeOrType
import org.jetbrains.kotlin.types.KotlinType

private fun getSuperTypesFromClass(ktClass: KtClass, context: BindingContext): List<KotlinType> {
  return ktClass.superTypeListEntries.mapNotNull { entry ->
    entry.typeReference?.getAbbreviatedTypeOrType(context)
  }
}

fun buildClassDeclaration(decl: KtClass, context: BindingContext): GleanClassDeclaration {
  val key =
      GleanClassDeclarationKey.Builder().apply {
        name = decl.qualifiedName()
        location = buildFileLocation(decl.psiOrParent)
        implements_ =
            getSuperTypesFromClass(decl, context).map { kotlinType -> kotlinType.path().toQName() }
      }
  return GleanClassDeclaration.Builder().setKey(key.build()).build()
}
