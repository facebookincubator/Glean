/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.kotlin_psi_utils

import com.facebook.tools.jast.primitiveToShortFormat
import glean.lang.kotlin.predicates.PathPredicate
import glean.lang.kotlin.predicates.QNamePredicate
import org.jetbrains.kotlin.descriptors.CallableDescriptor
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.PackageViewDescriptor
import org.jetbrains.kotlin.descriptors.ParameterDescriptor
import org.jetbrains.kotlin.descriptors.annotations.Annotations
import org.jetbrains.kotlin.descriptors.buildPossiblyInnerType
import org.jetbrains.kotlin.descriptors.containingPackage
import org.jetbrains.kotlin.descriptors.impl.PackageFragmentDescriptorImpl
import org.jetbrains.kotlin.js.descriptorUtils.getJetTypeFqName
import org.jetbrains.kotlin.js.descriptorUtils.nameIfStandardType
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.isFlexible
import org.jetbrains.kotlin.types.typeUtil.*
import org.jetbrains.kotlin.types.upperIfFlexible

private val DEFAULT_CONTAINER = "default"

fun KtClass.qualifiedName(): QNamePredicate? {
  return this.fqName?.pathSegments()?.map { segment -> segment.toString() }?.joinPath()?.toQName()
}

fun ParameterDescriptor.qualifiedName(): QNamePredicate {
  val methodDecl = this.containingDeclaration
  val container =
      if (methodDecl is CallableDescriptor) methodDecl.path() else DEFAULT_CONTAINER.path()
  return this.name.toString().qNameInPath(container)
}

fun CallableDescriptor.qualifiedName(): QNamePredicate {
  return this.path().toQName()
}

private fun CallableDescriptor.path(): PathPredicate {
  val methodPackage = this.containingPackage()
  val methodDecl = this.containingDeclaration
  val classPath =
      if (methodDecl is ClassDescriptor) {
        methodDecl.path()
      } else {
        methodPackage?.toString()?.path()
      }
  return this.name.toString().add(classPath)
}

fun ClassDescriptor.path(): PathPredicate {
  val containerPath: PathPredicate? =
      when (val container = this.containingDeclaration) {
        is ClassDescriptor -> container.path()
        is PackageViewDescriptor -> container.path()
        is PackageFragmentDescriptorImpl -> {
          if (container.fqName.isRoot) {
            null
          } else {
            container.fqName.path()
          }
        }
        else -> {
          null
        }
      }
  val container = containerPath ?: DEFAULT_CONTAINER.path()
  return this.name.toString().add(container)
}

fun FqName.path(): PathPredicate {
  return this.pathSegments().map { e -> e.toString() }.joinNonEmptyPath()
}

fun PackageViewDescriptor.path(): PathPredicate {
  return this.fqName.path()
}

fun String.path(): PathPredicate {
  return PathPredicate(this, null)
}

fun String.add(container: PathPredicate?): PathPredicate {
  return PathPredicate(this, container)
}

fun String.qNameInPath(path: PathPredicate): QNamePredicate {
  return QNamePredicate(this, path)
}

fun List<String>.joinPath(): PathPredicate? {
  var prev: PathPredicate? = null
  for (pathSegment in this) {
    prev = pathSegment.add(prev)
  }
  return prev
}

fun List<String>.joinNonEmptyPath(): PathPredicate {
  return this.joinPath() ?: throw Error("joinNonEmptyPath returned null")
}

fun PathPredicate.toQName(): QNamePredicate {
  return this.toQName { DEFAULT_CONTAINER.path() }
}

fun KotlinType.path(): PathPredicate {
  // we do not include annotations into names
  if (!this.annotations.isEmpty()) {
    return this.replaceAnnotations(Annotations.EMPTY).path()
  }

  if (this.isPrimitiveNumberType() || this.isBoolean() || this.isUnit()) {
    val typ = this.toString().lowercase()
    return primitiveToShortFormat(if (this.isMarkedNullable) typ.substringBefore("?") else typ)
        .path()
  } else {
    if (this.isTypeParameter() || this.isAnyOrNullableAny()) {
      return listOf("java", "lang", "Object").joinNonEmptyPath()
    }
    if (this.isArrayOfNothing()) {
      return listOf("java", "lang", "[Object").joinNonEmptyPath()
    }
    val potentialName = nameIfStandardType
    if (potentialName != null && potentialName.toString() == "Array") {
      val containedTypes = this.constituentTypes()
      if (containedTypes.size == 1) {
        val path = containedTypes.first().path()
        return path.amendBase { """[$it""" }
      }
    }
    if (this.isFlexible()) {
      return this.upperIfFlexible().path()
    }

    val fqNameMaybeNullable = this.getJetTypeFqName(false)
    val fqName =
        if (this.isMarkedNullable) fqNameMaybeNullable.substringBefore("?") else fqNameMaybeNullable
    val possiblyInnerType = this.buildPossiblyInnerType() ?: return fqName.path()
    return possiblyInnerType.classDescriptor.path()
  }
}
