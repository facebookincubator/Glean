/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer.glean_utils

import com.facebook.glean.schema.javakotlin_alpha.Name

fun buildName(string: String): Name {
  return Name.Builder().setKey(string).build()
}

fun String.toName(): Name {
  return Name.Builder().setKey(this).build()
}
