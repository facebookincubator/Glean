// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.ClassDeclaration;
import com.facebook.glean.schema.java_alpha.ClassDeclarationKey;
import com.facebook.glean.schema.java_alpha.ConstructorDeclaration;
import com.facebook.glean.schema.java_alpha.Declaration;
import com.facebook.glean.schema.java_alpha.DeclarationComment;
import com.facebook.glean.schema.java_alpha.Definition;
import com.facebook.glean.schema.java_alpha.FieldDeclaration;
import com.facebook.glean.schema.java_alpha.MethodDeclaration;
import com.facebook.glean.schema.java_alpha.Modifier;
import com.facebook.glean.schema.java_alpha.Type;
import com.facebook.glean.schema.java_alpha.TypeParam;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.ClassTree;
import java.util.List;

public class ClassDescriptor {
  public static ClassDeclaration describe(IndexerContext ic, ClassTree tree, Definition container) {
    ic.logger.indentedLog("ClassDeclaration");
    ic.logger.increaseIndent();

    QName qName = ClassUtils.buildName(ic, tree);

    List<Modifier> modifiers = ClassUtils.buildModifiers(ic, tree);
    List<Annotation> annotations = ClassUtils.buildAnnotations(ic, tree);
    Type extends_ = ClassUtils.buildExtends(ic, tree);
    List<Type> implements_ = ClassUtils.buildImplements(ic, tree);
    List<TypeParam> typeParams = ClassUtils.buildTypeParams(ic, tree);

    ByteSpan span = LocationDescriptor.getByteSpanOfTree(ic, tree);

    ic.logger.decreaseIndent();
    ic.logger.indentedLog("");

    ClassDeclarationKey key =
        new ClassDeclarationKey.Builder()
            .setName(qName)
            .setModifiers(modifiers)
            .setAnnotations(annotations)
            .setExtends_(extends_)
            .setImplements_(implements_)
            .setTypeParams(typeParams)
            .setContainer(container)
            .setSpan(span)
            .setFile(ClassUtils.buildFile(ic))
            .build();

    ClassDeclaration classDeclaration = new ClassDeclaration.Builder().setKey(key).build();
    Definition classDef = Definition.fromClass_(classDeclaration);
    ic.predicates.classDeclarationPredicate.addFact(classDeclaration);
    Declaration declaration = Declaration.fromClass_(classDeclaration);
    DeclarationComment commentDescriptor = CommentDescriptor.describe(ic, tree, declaration);

    List<FieldDeclaration> fields = ClassUtils.buildFields(ic, tree, classDef, false);
    List<ConstructorDeclaration> constructors = ClassUtils.buildConstructors(ic, tree, classDef);
    List<MethodDeclaration> methods = ClassUtils.buildMethods(ic, tree, classDef);
    List<Definition> innerDefinitions = ClassUtils.buildInnerDefinitions(ic, tree, classDef);

    return classDeclaration;
  }
}
