// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.PrimitiveType;
import com.facebook.glean.schema.java_alpha.PrimitiveTypeKey;

public class PrimitiveTypeDescriptor {
  public static PrimitiveType describe(IndexerContext ic, String type) {

    PrimitiveTypeKey primitiveKey = new PrimitiveTypeKey.Builder().setType(type).build();
    PrimitiveType _type = new PrimitiveType.Builder().setKey(primitiveKey).build();
    ic.logger.indentedLog("PrimitiveType: " + type);
    return _type;
  }
}
