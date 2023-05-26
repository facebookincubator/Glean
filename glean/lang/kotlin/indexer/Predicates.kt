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
import java.io.OutputStream

class Predicates {
  private val KOTLIN_SCHEMA_VERSION = 6
  private val KOTLIN_SCHEMA_NAME = "kotlin_alpha"
  private val METHOD_DECLARATION_PREDICATE_NAME =
      "$KOTLIN_SCHEMA_NAME.MethodDeclaration.$KOTLIN_SCHEMA_VERSION"

  private val predicatesMap = HashMap<Class<*>, MutableList<ThriftSerializable>>()

  private val predicateNames: HashMap<Class<*>, String> =
      hashMapOf(MethodDeclaration::class.java to METHOD_DECLARATION_PREDICATE_NAME)

  fun <T> addPredicate(value: T) where T : ThriftSerializable {
    val list = predicatesMap.getOrPut(value.javaClass) { mutableListOf() }
    list.add(value)
  }

  fun serialize(outputPath: String) {
    val out = FileOutputStream(outputPath)
    out.write("[".toByteArray())
    var first = true
    for ((type, name) in predicateNames) {
      val predicates = predicatesMap[type]
      if (!first) {
        out.write(",".toByteArray())
      }
      first = false

      serializePredicate(out, predicates as List<ThriftSerializable>, name)
    }
    out.write("]".toByteArray())
  }

  private fun serializePredicate(
      out: OutputStream,
      predicates: List<ThriftSerializable>,
      name: String
  ) {
    out.write("{".toByteArray())
    out.write("\"predicate\": \"$name\", ".toByteArray())
    out.write("\"facts\": ".toByteArray())

    out.write("[".toByteArray())
    // writing only last method for now
    predicates.forEachIndexed { index: Int, element: ThriftSerializable ->
      run {
        if (index > 0) {
          out.write(",".toByteArray())
        }
        SerializerUtil.toOutStream(element, out, SerializationProtocol.TSimpleJSONBase64)
      }
    }
    out.write("]".toByteArray())
    out.write("}".toByteArray())
  }
}
