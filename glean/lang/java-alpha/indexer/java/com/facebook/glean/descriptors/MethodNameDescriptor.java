// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.debug.DebugUtils;
import com.facebook.glean.descriptors.utils.SignatureGenerator;
import com.facebook.glean.schema.javakotlin_alpha.MethodName;
import com.facebook.glean.schema.javakotlin_alpha.MethodNameKey;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.javakotlin_alpha.Type;
import java.util.List;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.type.ExecutableType;

public class MethodNameDescriptor {

  public static MethodName describe(
      IndexerContext ic, ExecutableElement executableElement, boolean addToPredicate) {

    MethodName mName =
        MethodNameDescriptor.buildMethodName(
            QNameDescriptor.describe(ic, executableElement, addToPredicate),
            SignatureGenerator.assembleForTypes(
                ic, ((ExecutableType) executableElement.asType()).getParameterTypes()));
    if (addToPredicate) {
      MethodNameDescriptor.addToPredicate(ic, mName);
    }

    return mName;
  }

  private static MethodName buildMethodName(QName name, List<Type> signature) {
    MethodNameKey key = new MethodNameKey.Builder().setName(name).setSignature(signature).build();
    return new MethodName.Builder().setKey(key).build();
  }

  public static void addToPredicate(IndexerContext ic, MethodName mName) {
    DebugUtils.debugLogMethodName(ic, mName);
  }
}
