// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.AnnotationKey;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.AnnotationTree;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;

public class AnnotationDescriptor {
  public static Annotation describe(IndexerContext ic, AnnotationTree annotationTree) {
    Element annotationElement =
        ic.trees.getElement(ic.trees.getPath(ic.cu, annotationTree.getAnnotationType()));
    if (annotationElement == null) {
      throw new DescriptorException(
          annotationTree.toString() + " annotation did not have a viable element");
    }
    QName qName = QNameDescriptor.describe(ic, (TypeElement) annotationElement, true);
    // Todo add methodName for constructor used, add optional strings if present
    ByteSpan location = LocationDescriptor.getByteSpanOfTree(ic, annotationTree);
    AnnotationKey key = new AnnotationKey.Builder().setName(qName).setSpan(location).build();
    return new Annotation.Builder().setKey(key).build();
  }
}
