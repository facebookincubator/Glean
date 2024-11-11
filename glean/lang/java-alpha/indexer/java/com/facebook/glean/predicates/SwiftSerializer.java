// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.predicates;

import static com.facebook.thrift.util.SerializationProtocol.TSimpleJSON;

import com.facebook.thrift.payload.ThriftSerializable;
import com.facebook.thrift.util.SerializerUtil;

class SwiftSerializer {
  public static byte[] serializeJSON(ThriftSerializable obj) {
    if (obj == null) {
      return null;
    }

    return SerializerUtil.toByteArray(obj, TSimpleJSON);
  }
}
