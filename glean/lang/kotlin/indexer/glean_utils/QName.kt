/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.javakotlin_alpha.QName
import com.facebook.glean.schema.javakotlin_alpha.QNameKey
import org.jetbrains.kotlin.name.FqName

fun buildQName(fqName: FqName): QName {
  val key = QNameKey.Builder().build()
  return QName.Builder().setKey(key).build()
}

fun buildQName(name: String, fqName: FqName): QName {
  val key = QNameKey.Builder().build()
  return QName.Builder().setKey(key).build()
}
