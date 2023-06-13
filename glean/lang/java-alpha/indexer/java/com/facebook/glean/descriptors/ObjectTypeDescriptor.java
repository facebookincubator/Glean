// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.ObjectType;
import com.facebook.glean.schema.java_alpha.ObjectTypeKey;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.Tree;
import javax.lang.model.element.TypeElement;

public class ObjectTypeDescriptor {
  public static ObjectType describe(
      IndexerContext ic, Tree source, TypeElement type, ByteSpan optSpan) {
    ByteSpan span =
        (ic.isCompilerGenerated(source) && !(optSpan == null))
            ? optSpan
            : LocationDescriptor.getByteSpanOfTree(ic, source);
    QName typeName = QNameDescriptor.describe(ic, type, true);
    ObjectTypeKey key = new ObjectTypeKey.Builder().setSpan(span).setType(typeName).build();
    ObjectType _type = new ObjectType.Builder().setKey(key).build();
    ic.logger.indentedLog("Declared Type:");
    return _type;
  }
}
