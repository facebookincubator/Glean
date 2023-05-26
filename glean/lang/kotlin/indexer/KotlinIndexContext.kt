/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

import com.facebook.glean.schema.kotlin_alpha.MethodDeclaration

class KotlinIndexContext {
  private val predicates = Predicates()

  fun addMethods(method: MethodDeclaration) {
    this.predicates.addPredicate(method)
  }

  fun dump(outputPath: String) {
    this.predicates.serialize(outputPath)
  }
}
