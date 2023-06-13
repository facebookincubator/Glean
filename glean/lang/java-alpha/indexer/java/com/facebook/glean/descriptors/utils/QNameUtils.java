// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors.utils;

import com.facebook.glean.schema.javakotlin_alpha.QName;

public class QNameUtils {
  public static boolean hasFqName(QName name) {
    String qualifiedName = name.getKey().toString();
    return qualifiedName != null && !qualifiedName.isEmpty();
  }
}
