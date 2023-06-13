// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.PackageDeclaration;
import com.facebook.glean.schema.java_alpha.PackageDeclarationKey;
import com.facebook.glean.schema.javakotlin_alpha.Path;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.PackageTree;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class PackageDescriptor {

  public static PackageDeclaration describe(IndexerContext ic, PackageTree tree) {
    ic.logger.indentedLog("PackageDeclaration");
    ic.logger.increaseIndent();

    PackageDeclarationKey.Builder key = new PackageDeclarationKey.Builder();
    String name = tree.getPackageName().toString();
    PathDescriptor.NameAndPath nameAndPath = PathDescriptor.structurePath(ic, name);
    Path path = PathDescriptor.describe(ic, nameAndPath.simple, nameAndPath.path);
    ByteSpan span = LocationDescriptor.getByteSpanOfTree(ic, tree);
    ic.logger.indentedLog("Name : " + name);
    key.setName(NameDescriptor.describe(ic, name));
    key.setPath(path);
    key.setLocation(span);

    ic.logger.indentedLog("Annotations");
    ic.logger.increaseIndent();
    if (tree.getAnnotations() != null) {
      List<Annotation> annotations =
          tree.getAnnotations().stream()
              .map(annotationTree -> AnnotationDescriptor.describe(ic, annotationTree))
              .collect(Collectors.toList());
      key.setAnnotation(annotations);
    } else {
      key.setAnnotation(new ArrayList<Annotation>());
    }
    ic.logger.decreaseIndent();

    ic.logger.decreaseIndent();
    ic.logger.indentedLog("");

    PackageDeclaration packageDeclaration =
        new PackageDeclaration.Builder().setKey(key.build()).build();

    ic.predicates.packageDeclarationPredicate.addFact(packageDeclaration);
    return packageDeclaration;
  }
}
