// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.ImportDeclaration;
import com.facebook.glean.schema.java_alpha.ImportDeclarationKey;
import com.facebook.glean.schema.javakotlin_alpha.Path;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.ImportTree;

public class ImportDescriptor {

  public static ImportDeclaration describe(IndexerContext ic, ImportTree tree) {
    ic.logger.indentedLog("ImportDeclaration");
    ic.logger.increaseIndent();

    String path = tree.getQualifiedIdentifier().toString();
    String typePath;
    String staticMember = null;
    boolean onDemand = false;

    ImportDeclarationKey.Builder key = new ImportDeclarationKey.Builder();

    if (tree.isStatic()) {
      int lastDotDivider = path.lastIndexOf(".");
      typePath = path.substring(0, lastDotDivider);
      staticMember = path.substring(lastDotDivider + 1, path.length());
      onDemand = staticMember.equals("*");
    } else if (path.endsWith("*")) {
      int lastDotDivider = path.lastIndexOf(".");
      typePath = path.substring(0, lastDotDivider);
      onDemand = true;
    } else {
      typePath = path;
    }

    ic.logger.indentedLog("Name : " + typePath);
    ic.logger.indentedLog("isImportOnDemand : " + (onDemand ? "true" : "false"));
    ic.logger.indentedLog("isStatic : " + (staticMember != null ? "true" : "false"));
    if (staticMember != null) {
      ic.logger.increaseIndent();
      ic.logger.indentedLog("Static member: " + staticMember);
      ic.logger.decreaseIndent();
      key.setStaticMember(staticMember);
    }
    key.setName(NameDescriptor.describe(ic, typePath));
    key.setImportOnDemand(onDemand);
    ic.logger.increaseIndent();
    ic.logger.indentedLog("Structure path:");
    PathDescriptor.NameAndPath nameAndPath = PathDescriptor.structurePath(ic, typePath);
    Path structure_path = PathDescriptor.describe(ic, nameAndPath.simple, nameAndPath.path);
    ic.logger.decreaseIndent();
    key.setPath(structure_path);

    ByteSpan span = LocationDescriptor.getByteSpanOfTree(ic, tree);
    key.setLocation(span);

    ic.logger.decreaseIndent();
    ic.logger.indentedLog("");

    ImportDeclaration importDeclaration =
        new ImportDeclaration.Builder().setKey(key.build()).build();

    ic.predicates.importDeclarationPredicate.addFact(importDeclaration);
    return importDeclaration;
  }
}
