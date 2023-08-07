// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.Declaration;
import com.facebook.glean.schema.java_alpha.DeclarationComment;
import com.facebook.glean.schema.java_alpha.Definition;
import com.facebook.glean.schema.java_alpha.EnumDeclaration;
import com.facebook.glean.schema.java_alpha.EnumDeclarationKey;
import com.facebook.glean.schema.java_alpha.FieldDeclaration;
import com.facebook.glean.schema.java_alpha.MethodDeclaration;
import com.facebook.glean.schema.java_alpha.Modifier;
import com.facebook.glean.schema.java_alpha.Type;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.ClassTree;
import java.util.List;

public class EnumDescriptor {
  public static EnumDeclaration describe(IndexerContext ic, ClassTree tree, Definition container) {
    ic.logger.indentedLog("EnumDeclaration");
    ic.logger.increaseIndent();

    QName qName = ClassUtils.buildName(ic, tree);

    List<Modifier> modifiers = ClassUtils.buildModifiers(ic, tree);
    List<Annotation> annotations = ClassUtils.buildAnnotations(ic, tree);
    List<Type> implements_ = ClassUtils.buildImplements(ic, tree);
    ByteSpan span = LocationDescriptor.getByteSpanOfTree(ic, tree);

    ic.logger.decreaseIndent();
    ic.logger.indentedLog("");

    EnumDeclarationKey key =
        new EnumDeclarationKey.Builder()
            .setName(qName)
            .setModifiers(modifiers)
            .setAnnotations(annotations)
            .setImplements_(implements_)
            .setContainer(container)
            .setSpan(span)
            .setFile(ClassUtils.buildFile(ic))
            .build();

    EnumDeclaration enumDeclaration = new EnumDeclaration.Builder().setKey(key).build();
    ic.predicates.enumDeclarationPredicate.addFact(enumDeclaration);
    Definition enumDef = Definition.fromEnum_(enumDeclaration);
    Declaration declaration = Declaration.fromEnum_(enumDeclaration);
    DeclarationComment commentDescriptor = CommentDescriptor.describe(ic, tree, declaration);

    List<FieldDeclaration> variables = ClassUtils.buildFields(ic, tree, enumDef, true);
    List<MethodDeclaration> methods = ClassUtils.buildMethods(ic, tree, enumDef);
    List<Definition> innerDefinitions = ClassUtils.buildInnerDefinitions(ic, tree, enumDef);

    return enumDeclaration;
  }
}
