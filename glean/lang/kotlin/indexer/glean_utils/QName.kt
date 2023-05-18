/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.javakotlin_alpha.*
import com.facebook.tools.jast.primitiveToShortFormat
import org.jetbrains.kotlin.descriptors.*
import org.jetbrains.kotlin.js.descriptorUtils.getJetTypeFqName
import org.jetbrains.kotlin.js.descriptorUtils.nameIfStandardType
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.isFlexible
import org.jetbrains.kotlin.types.typeUtil.*
import org.jetbrains.kotlin.types.upperIfFlexible

fun ParameterDescriptor.qualifiedName(): QName {
  val methodDecl = this.containingDeclaration
  val container = if (methodDecl is CallableDescriptor) methodDecl.path() else null
  return container.qNameInPath(this.name.toString())
}

private fun CallableDescriptor.qualifiedName(): QName {
  return this.path().toQName()
}

private fun CallableDescriptor.path(): Path {
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

fun ClassDescriptor.path(): Path {
  val container = this.containingDeclaration
  val containerPath: Path? =
      if (container is ClassDescriptor) container.path()
      else {
        if (container is PackageViewDescriptor) {
          container.path()
        } else {
          null
        }
      }
  return containerPath.add(this.name.toString())
}

fun PackageViewDescriptor.path(): Path {
  return this.fqName.pathSegments().map { e -> e.toString() }.joinNonEmptyPath()
}

fun String.path(): Path {
  val key = PathKey.Builder().setBase(this.toName()).build()
  return Path.Builder().setKey(key).build()
}

fun Path?.add(name: String): Path {
  val key = PathKey.Builder().setBase(name.toName())
  if (this !== null) {
    key.container = this
  }
  return Path.Builder().setKey(key.build()).build()
}

fun Path?.qNameInPath(name: String): QName {
  val key = QNameKey.Builder().setName(name.toName())
  if (this !== null) {
    key.context = this
  }
  return QName.Builder().setKey(key.build()).build()
}

fun List<String>.joinPath(): Path? {
  var prev: Path? = null
  for (path in this) {
    prev = prev?.add(path)
  }
  return prev
}

fun List<String>.joinNonEmptyPath(): Path {
  return this.joinPath() ?: throw Error("joinNonEmptyPath returned null")
}

fun Path.replaceLast(key: String): Path {
  return this.key?.container.add(key)
}

fun Path.toQName(): QName {
  return this.key?.container.qNameInPath(this.key?.base.toString())
}

fun KotlinType.path(): Path {
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
