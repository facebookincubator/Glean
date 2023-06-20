/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.javakotlin_alpha.Path as GleanPath
import com.facebook.glean.schema.javakotlin_alpha.PathKey as GleanPathKey
import com.facebook.glean.schema.javakotlin_alpha.QName
import com.facebook.glean.schema.javakotlin_alpha.QNameKey
import com.facebook.tools.jast.primitiveToShortFormat
import org.jetbrains.kotlin.descriptors.*
import org.jetbrains.kotlin.descriptors.annotations.Annotations
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

fun KtClass.qualifiedName(): QName? {
  return this.fqName?.pathSegments()?.map { segment -> segment.toString() }?.joinPath()?.toQName()
}

fun ParameterDescriptor.qualifiedName(): QName {
  val methodDecl = this.containingDeclaration
  val container =
      if (methodDecl is CallableDescriptor) methodDecl.path() else DEFAULT_CONTAINER.path()
  return container.qNameInPath(this.name.toString())
}

fun CallableDescriptor.qualifiedName(): QName {
  return this.path().toQName()
}

private fun CallableDescriptor.path(): GleanPath {
  val methodPackage = this.containingPackage()
  val methodDecl = this.containingDeclaration
  val classPath =
      if (methodDecl is ClassDescriptor) {
        methodDecl.path()
      } else {
        methodPackage?.toString()?.path()
      }
  return classPath.add(this.name.toString())
}

fun ClassDescriptor.path(): GleanPath {
  val containerPath: GleanPath? =
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
  return container.add(this.name.toString())
}

fun FqName.path(): GleanPath {
  return this.pathSegments().map { e -> e.toString() }.joinNonEmptyPath()
}

fun PackageViewDescriptor.path(): GleanPath {
  return this.fqName.path()
}

fun String.path(): GleanPath {
  val key = GleanPathKey.Builder().setBase(this.toName()).build()
  return GleanPath.Builder().setKey(key).build()
}

fun GleanPath?.add(name: String): GleanPath {
  val key = GleanPathKey.Builder().setBase(name.toName())
  if (this !== null) {
    key.container = this
  }
  return GleanPath.Builder().setKey(key.build()).build()
}

fun GleanPath.qNameInPath(name: String): QName {
  val key = QNameKey.Builder().setName(name.toName())
  key.context = this
  return QName.Builder().setKey(key.build()).build()
}

fun List<String>.joinPath(): GleanPath? {
  var prev: GleanPath? = null
  for (pathSegment in this) {
    prev = prev?.add(pathSegment) ?: pathSegment.path()
  }
  return prev
}

fun List<String>.joinNonEmptyPath(): GleanPath {
  return this.joinPath() ?: throw Error("joinNonEmptyPath returned null")
}

fun GleanPath.replaceLast(key: String): GleanPath {
  return this.key?.container.add(key)
}

fun GleanPath.toQName(): QName {
  val container = this.key?.container ?: DEFAULT_CONTAINER.path()
  return container.qNameInPath(this.key?.base.toString())
}

fun KotlinType.path(): GleanPath {
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
        return path.replaceLast("""[${path.key?.base?.key}""")
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
