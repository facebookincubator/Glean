/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

import glean.lang.kotlin.predicates.MethodDeclarationPredicate
import glean.lang.kotlin.predicates.Predicates

class KotlinIndexContext {
  private val predicates = Predicates()

  fun addMethods(method: MethodDeclarationPredicate) {
    this.predicates.addPredicate(method)
  }

  fun dump(outputPath: String) {
    this.predicates.serialize(outputPath)
  }
}
