/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package glean.lang.kotlin.indexer

import com.facebook.glean.schema.kotlin_alpha.MethodDeclaration
import com.facebook.thrift.payload.ThriftSerializable
import com.facebook.thrift.util.SerializationProtocol
import com.facebook.thrift.util.SerializerUtil
import java.io.FileOutputStream

class KotlinIndexContext {
  private val methods = mutableListOf<MethodDeclaration>()

  fun addMethods(method: MethodDeclaration) {
    this.methods.add(method)
  }

  fun dump(outputPath: String) {
    if (this.methods.size > 0) { 
        val out = FileOutputStream(outputPath) 
        out.write("[".toByteArray())
      // writing only last method for now
        this.methods.forEachIndexed { index: Int, element: ThriftSerializable ->
            run {
                if (index > 0) {
                    out.write(",".toByteArray())
                }
                SerializerUtil.toOutStream(
                    element,
                    out,
                    SerializationProtocol.TSimpleJSONBase64
                )
            }
        }
        out.write("]".toByteArray())
    }
  }
}
