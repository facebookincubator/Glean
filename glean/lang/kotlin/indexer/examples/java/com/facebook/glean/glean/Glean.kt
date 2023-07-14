// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.glean

open class SuperGlean {}

class Glean : SuperGlean {

  constructor() : super()

  fun fetch(): ClassA {
    return ClassA()
  }

  companion object {
    const val LOG_TAG = "Glean"

    @JvmStatic
    fun main(args: Array<String>) {
      val glean = Glean()
      val instance = glean.fetch()
      instance.e
      instance.getE(10)
      instance.getList(null)
    }
  }
}
