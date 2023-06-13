// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.predicates;

import com.facebook.swift.codec.ThriftCodecManager;
import java.io.ByteArrayOutputStream;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TTransport;

class SwiftSerializer {

  // This file was inspired from
  // fbcode/rtgw/mqtt/src/test/java/com/facebook/rtgw/handlers/delivery/processors/RealtimePresenceDeliveryProcessorTest.java
  // https://fburl.com/diffusion/765mst67

  private static final ThriftCodecManager thriftCodecManager = new ThriftCodecManager();
  private static final TProtocolFactory jsonProtocolFactory = new TSimpleJSONProtocol.Factory();

  public static <T> byte[] serializeJSON(T obj) throws RuntimeException {
    if (obj == null) {
      return null;
    }

    ByteArrayOutputStream byteArrayOutput = new ByteArrayOutputStream();
    TTransport outStreamTransport = new TIOStreamTransport(byteArrayOutput);
    TProtocol protocol = new TSimpleJSONProtocol(outStreamTransport);

    try {
      thriftCodecManager.write((Class<T>) obj.getClass(), obj, protocol);
    } catch (Exception e) {
      throw new RuntimeException(e);
    } finally {
      outStreamTransport.close();
    }

    return byteArrayOutput.toByteArray();
  }
}
