// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.utils.QNameUtils;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.Definition;
import com.facebook.glean.schema.java_alpha.FieldDeclaration;
import com.facebook.glean.schema.java_alpha.InterfaceDeclaration;
import com.facebook.glean.schema.java_alpha.InterfaceDeclarationKey;
import com.facebook.glean.schema.java_alpha.MethodDeclaration;
import com.facebook.glean.schema.java_alpha.Modifier;
import com.facebook.glean.schema.java_alpha.Type;
import com.facebook.glean.schema.java_alpha.TypeParam;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.ClassTree;
import java.util.List;
import java.util.Map;

public class InterfaceDescriptor {
  public static InterfaceDeclaration describe(
      IndexerContext ic,
      ClassTree tree,
      Definition container,
      Map<String, Definition> existingDefinitionMap) {
    ic.logger.indentedLog("InterfaceDeclaration");
    ic.logger.increaseIndent();

    QName qName = ClassUtils.buildName(ic, tree);

    if (QNameUtils.hasFqName(qName)) {
      if (existingDefinitionMap.containsKey(qName.getKey().toString())) {
        return existingDefinitionMap.get(qName.getKey().toString()).getInterface_();
      }
    }

    List<Modifier> modifiers = ClassUtils.buildModifiers(ic, tree);
    List<Annotation> annotations = ClassUtils.buildAnnotations(ic, tree);
    List<Type> extends_ = ClassUtils.buildImplements(ic, tree);
    List<TypeParam> typeParams = ClassUtils.buildTypeParams(ic, tree);

    ByteSpan span = LocationDescriptor.getByteSpanOfTree(ic, tree);

    ic.logger.decreaseIndent();
    ic.logger.indentedLog("");

    InterfaceDeclarationKey key =
        new InterfaceDeclarationKey.Builder()
            .setName(qName)
            .setModifiers(modifiers)
            .setAnnotations(annotations)
            .setExtends_(extends_)
            .setTypeParams(typeParams)
            .setContainer(container)
            .setSpan(span)
            .setFile(ClassUtils.buildFile(ic))
            .build();

    InterfaceDeclaration interfaceDeclaration =
        new InterfaceDeclaration.Builder().setKey(key).build();
    ic.predicates.interfaceDeclarationPredicate.addFact(interfaceDeclaration);
    Definition interfaceDef = Definition.fromInterface_(interfaceDeclaration);
    if (QNameUtils.hasFqName(qName)) {
      existingDefinitionMap.put(qName.getKey().toString(), interfaceDef);
    }

    List<MethodDeclaration> methods = ClassUtils.buildMethods(ic, tree, interfaceDef);
    List<FieldDeclaration> variables = ClassUtils.buildFields(ic, tree, interfaceDef, false);
    List<Definition> innerDefinitions =
        ClassUtils.buildInnerDefinitions(ic, tree, interfaceDef, existingDefinitionMap);

    return interfaceDeclaration;
  }
}
