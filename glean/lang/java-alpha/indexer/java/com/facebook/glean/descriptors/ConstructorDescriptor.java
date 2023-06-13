// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.ConstructorDeclaration;
import com.facebook.glean.schema.java_alpha.ConstructorDeclarationKey;
import com.facebook.glean.schema.java_alpha.Definition;
import com.facebook.glean.schema.java_alpha.Modifier;
import com.facebook.glean.schema.java_alpha.ParameterDeclaration;
import com.facebook.glean.schema.java_alpha.Type;
import com.facebook.glean.schema.java_alpha.TypeParam;
import com.facebook.glean.schema.javakotlin_alpha.MethodName;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.ModifiersTree;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;

public class ConstructorDescriptor {

  public static ConstructorDeclaration describe(
      IndexerContext ic, MethodTree tree, Definition container) {
    // We use the element kind to ensure that only constructors are described
    ic.logger.indentedLog("ConstructorDeclaration");
    ic.logger.increaseIndent();

    Element methodElement = ic.trees.getElement(ic.trees.getPath(ic.cu, tree));
    if (methodElement == null) {
      throw new DescriptorException(
          tree.getName().toString() + " constructor does not have viable element");
    }

    ic.logger.indentedLog("Name");
    ic.logger.increaseIndent();
    MethodName mName = MethodNameDescriptor.describe(ic, (ExecutableElement) methodElement, true);
    ic.logger.decreaseIndent();
    ic.logger.indentedLog(ic.isCompilerGenerated(tree) ? "Is generated" : "is not generated");

    ModifiersTree modifiersTree = tree.getModifiers();
    ic.logger.indentedLog("Modifiers");
    ic.logger.increaseIndent();
    List<Modifier> modifiers = ModifierDescriptor.describe(ic, modifiersTree);
    ic.logger.decreaseIndent();

    ic.logger.indentedLog("Annotations");
    ic.logger.increaseIndent();
    List<Annotation> annotations =
        modifiersTree.getAnnotations().stream()
            .map(annotationTree -> AnnotationDescriptor.describe(ic, annotationTree))
            .collect(Collectors.toList());
    ic.logger.decreaseIndent();

    ic.logger.indentedLog("Parameters");
    ic.logger.increaseIndent();
    List<ParameterDeclaration> parameters =
        tree.getParameters().stream()
            .map(param -> VariableDescriptor.describeParameter(ic, param))
            .collect(Collectors.toList());
    ic.logger.decreaseIndent();

    ic.logger.indentedLog("TypeParams");
    ic.logger.increaseIndent();
    List<TypeParam> typeParams = TypeDescriptor.describeTypeParams(ic, tree.getTypeParameters());
    ic.logger.decreaseIndent();

    ic.logger.indentedLog("Throws");
    ic.logger.increaseIndent();
    // Although ExpressionTree, these should each be an Identifier form so should not be inspected
    List<Type> throws_ = TypeDescriptor.describeTypes(ic, tree.getThrows());
    ic.logger.decreaseIndent();

    ByteSpan span = LocationDescriptor.getByteSpanOfTree(ic, tree);

    ic.logger.decreaseIndent();

    ConstructorDeclarationKey key =
        new ConstructorDeclarationKey.Builder()
            .setName(mName)
            .setParameters(parameters)
            .setModifiers(modifiers)
            .setAnnotations(annotations)
            .setTypeParams(typeParams)
            .setThrows_(throws_)
            .setContainer(container)
            .setSpan(span)
            .build();

    ConstructorDeclaration constructor = new ConstructorDeclaration.Builder().setKey(key).build();
    ic.predicates.constructorDeclarationPredicate.addFact(constructor);
    return constructor;
  }
}
