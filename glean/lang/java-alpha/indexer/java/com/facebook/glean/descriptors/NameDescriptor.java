// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.javakotlin_alpha.Name;

public class NameDescriptor {
  public static Name describe(IndexerContext ic, String name) {
    Name _name = new Name.Builder().setKey(name).build();
    ic.logger.indentedLog("Name: " + name);
    return _name;
  }
}
