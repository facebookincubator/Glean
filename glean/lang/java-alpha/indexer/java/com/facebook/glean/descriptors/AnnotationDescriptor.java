// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.descriptors.utils.SignatureGenerator;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.AnnotationKey;
import com.facebook.glean.schema.javakotlin_alpha.MethodName;
import com.facebook.glean.schema.javakotlin_alpha.MethodNameKey;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.javakotlin_alpha.Type;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.AnnotationTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.LiteralTree;
import com.sun.source.tree.Tree;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;

public class AnnotationDescriptor {
  public static Annotation describe(IndexerContext ic, AnnotationTree annotationTree) {
    Element annotationElement =
        ic.trees.getElement(ic.trees.getPath(ic.cu, annotationTree.getAnnotationType()));
    if (annotationElement == null) {
      throw new DescriptorException(
          annotationTree.toString() + " annotation did not have a viable element");
    }
    List<? extends ExpressionTree> arguments = annotationTree.getArguments();
    List<Type> signature = new ArrayList<>();
    try {
      List<? extends TypeMirror> argumentMirrors =
          arguments.stream()
              .map(
                  argument ->
                      (TypeMirror) ic.trees.getElement(ic.trees.getPath(ic.cu, argument)).asType())
              .collect(Collectors.toList());
      signature = SignatureGenerator.assembleForTypes(ic, argumentMirrors);
    } catch (NullPointerException e) {
      // Add logging? This will happen if Element is null
      signature = new ArrayList<>();
    }
    QName qName = QNameDescriptor.describe(ic, (TypeElement) annotationElement, true);
    MethodName mName =
        new MethodName.Builder()
            .setKey(new MethodNameKey.Builder().setName(qName).setSignature(signature).build())
            .build();
    String constant = null;
    if (arguments.size() == 1) {
      if (arguments.get(0).getKind() == Tree.Kind.STRING_LITERAL) {
        constant = (String) ((LiteralTree) arguments.get(0)).getValue();
      }
    }
    // Todo add methodName for constructor used, add optional strings if present
    ByteSpan location = LocationDescriptor.getByteSpanOfTree(ic, annotationTree);
    AnnotationKey key =
        new AnnotationKey.Builder()
            .setName(qName)
            .setSpan(location)
            .setConstant(constant)
            .setConstructor(mName)
            .build();
    return new Annotation.Builder().setKey(key).build();
  }
}
