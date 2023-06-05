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
  private val KOTLIN_SCHEMA_VERSION = 1
  private val KOTLIN_SCHEMA_NAME = "kotlin.alpha"
  private val METHOD_DECLARATION_PREDICATE_NAME =
      "$KOTLIN_SCHEMA_NAME.MethodDeclaration.$KOTLIN_SCHEMA_VERSION"

  private val predicatesMap = HashMap<Class<*>, MutableList<ThriftSerializable>>()

  private val predicateNames: HashMap<Class<*>, String> =
      hashMapOf(MethodDeclaration::class.java to METHOD_DECLARATION_PREDICATE_NAME)

  fun <T> addPredicate(value: T) where T : ThriftSerializable {
    val list = predicatesMap.getOrPut(value.javaClass) { mutableListOf() }
    list.add(value)
  }

  fun OutputStream.writeBites(text: String) {
    write(text.toByteArray())
  }

  fun serialize(outputPath: String) {
    with(FileOutputStream(outputPath)) {
      writeBites("[")
      var first = true
      for ((type, name) in predicateNames) {
        val predicates = predicatesMap[type] ?: continue
        if (!first) {
          writeBites(",")
        }
        first = false

        serializePredicate(this, predicates as List<ThriftSerializable>, name)
      }
      writeBites("]")
    }
  }

  private fun serializePredicate(
      out: OutputStream,
      predicates: List<ThriftSerializable>,
      name: String
  ) {
    with(out) {
      writeBites("{")
      writeBites("\"predicate\":\"$name\",")
      writeBites("\"facts\":")

      writeBites("[")
      // writing only last method for now
      predicates.forEachIndexed { index: Int, element: ThriftSerializable ->
        run {
          if (index > 0) {
            writeBites(",")
          }
          SerializerUtil.toOutStream(element, out, SerializationProtocol.TSimpleJSONBase64)
        }
      }
      writeBites("]")
      writeBites("}")
    }
  }
}
