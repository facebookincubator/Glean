// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.glean

class ClassA {
  companion object {
    const val a = 1
    private const val b = 2
    protected const val c = 3
    internal const val d = 4
  }

  var e = "e"
  private var f = "f"
  protected var g = "g"
  internal var h = "h"

  var i = Array(5) { Array(5) { BooleanArray(5) } }
  var aList: List<*>? = null

  constructor() : this("eDefaultCtor", "fDefaultCtor", "gDefaultCtor", "hDefaultCtor")

  constructor(e: String, f: String, g: String, h: String) {
    this.e = e
    this.f = f
    this.g = g
    this.h = h
  }

  fun getList(clazz: List<Class<*>>?) {
    val x = 10
    return
  }

  fun getE(randomArg: Int): String {
    return e
  }

  fun isInstanceOfList(`object`: Any): Boolean {
    return `object` is List<*>
  }
}
