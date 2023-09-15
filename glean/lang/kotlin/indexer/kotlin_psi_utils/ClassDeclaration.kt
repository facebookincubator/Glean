/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

import glean.lang.kotlin.predicates.ClassDeclarationPredicate
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.bindingContextUtil.getAbbreviatedTypeOrType
import org.jetbrains.kotlin.types.KotlinType

private fun getSuperTypesFromClass(ktClass: KtClass, context: BindingContext): List<KotlinType> {
  return ktClass.superTypeListEntries.mapNotNull { entry ->
    entry.typeReference?.getAbbreviatedTypeOrType(context)
  }
}

fun buildClassDeclaration(decl: KtClass, context: BindingContext): ClassDeclarationPredicate {
  return ClassDeclarationPredicate(
      decl.qualifiedName() ?: throw MissingRequiredGleanFieldException("ClassDeclaration.name"),
      implements =
          getSuperTypesFromClass(decl, context).map { kotlinType -> kotlinType.path().toQName() },
      location =
          buildFileLocation(decl.psiOrParent)
              ?: throw EmptyDeclarationLocation(decl.name ?: "", "class"))
}
