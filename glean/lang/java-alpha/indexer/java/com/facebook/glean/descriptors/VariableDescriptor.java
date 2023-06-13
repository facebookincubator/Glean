// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.descriptors.utils.TypeUtils;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.Definition;
import com.facebook.glean.schema.java_alpha.ExecutableDefinition;
import com.facebook.glean.schema.java_alpha.FieldDeclaration;
import com.facebook.glean.schema.java_alpha.FieldDeclarationKey;
import com.facebook.glean.schema.java_alpha.LocalDeclaration;
import com.facebook.glean.schema.java_alpha.LocalDeclarationKey;
import com.facebook.glean.schema.java_alpha.Modifier;
import com.facebook.glean.schema.java_alpha.ParameterDeclaration;
import com.facebook.glean.schema.java_alpha.ParameterDeclarationKey;
import com.facebook.glean.schema.java_alpha.Type;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.ModifiersTree;
import com.sun.source.tree.VariableTree;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.VariableElement;

public class VariableDescriptor {

  private static ByteSpan variableSpan(IndexerContext ic, VariableTree tree) {
    // If it is compiler generated, and still here, it is likely an enum field
    // look for source in identifier to use for this and potentially type
    ByteSpan span;
    if (ic.isCompilerGenerated(tree)) {
      span = LocationDescriptor.getByteSpanOfTree(ic, tree.getNameExpression());
    } else {
      span = LocationDescriptor.getByteSpanOfTree(ic, tree);
    }
    return span;
  }

  private static VariableElement variableElement(IndexerContext ic, VariableTree tree) {
    Element vElement = ic.trees.getElement(ic.trees.getPath(ic.cu, tree));
    if (vElement == null) {
      throw new DescriptorException(tree.toString() + " does not have a valid element");
    }
    if (vElement instanceof VariableElement) {
      return (VariableElement) vElement;
    } else {
      throw new DescriptorException(
          ic, vElement, tree.toString() + " element is not a VariableElement");
    }
  }

  private static Type variableType(IndexerContext ic, VariableTree tree, ByteSpan span) {
    ic.logger.indentedLog("Type");
    ic.logger.increaseIndent();

    Type type =
        TypeDescriptor.describeType(ic, tree.getType(), TypeUtils.GetTypeMirror(ic, tree), span);
    ic.logger.decreaseIndent();
    return type;
  }

  private static List<Modifier> variableModifiers(IndexerContext ic, VariableTree tree) {
    ModifiersTree modifiersTree = tree.getModifiers();
    ic.logger.indentedLog("Modifiers");
    ic.logger.increaseIndent();
    List<Modifier> modifiers = ModifierDescriptor.describe(ic, modifiersTree);
    ic.logger.decreaseIndent();
    return modifiers;
  }

  private static List<Annotation> variableAnnotations(IndexerContext ic, VariableTree tree) {
    ModifiersTree modifiersTree = tree.getModifiers();
    ic.logger.indentedLog("Annotations");
    ic.logger.increaseIndent();
    List<Annotation> annotations =
        modifiersTree.getAnnotations().stream()
            .map(annotationTree -> AnnotationDescriptor.describe(ic, annotationTree))
            .collect(Collectors.toList());
    ic.logger.decreaseIndent();
    return annotations;
  }

  public static FieldDeclaration describeField(
      IndexerContext ic, VariableTree tree, Definition container) {
    ic.logger.indentedLog("VariableDeclaration");
    ic.logger.increaseIndent();

    ic.logger.indentedLog("Field definition");
    ic.logger.increaseIndent();

    ByteSpan span = variableSpan(ic, tree);
    VariableElement variableElement = variableElement(ic, tree);

    QName qName = QNameDescriptor.describe(ic, variableElement, true);
    ic.logger.decreaseIndent();

    Type type = variableType(ic, tree, span);
    List<Modifier> modifiers = variableModifiers(ic, tree);
    List<Annotation> annotations = variableAnnotations(ic, tree);

    if (variableElement.getKind() == ElementKind.LOCAL_VARIABLE
        || variableElement.getKind() == ElementKind.PARAMETER) {
      throw new DescriptorException(
          tree.toString()
              + " expected field variable found local with kind "
              + variableElement.getKind());
    }

    FieldDeclarationKey key =
        new FieldDeclarationKey.Builder()
            .setName(qName)
            .setType(type)
            .setModifiers(modifiers)
            .setAnnotations(annotations)
            .setSpan(span)
            .setContainer(container)
            .build();

    FieldDeclaration field = new FieldDeclaration.Builder().setKey(key).build();
    ic.predicates.fieldDeclarationPredicate.addFact(field);
    return field;
  }

  public static LocalDeclaration describeLocal(
      IndexerContext ic, VariableTree tree, ExecutableDefinition eContainer) {
    ic.logger.indentedLog("VariableDeclaration");
    ic.logger.increaseIndent();

    ic.logger.indentedLog("Local definition");
    ic.logger.increaseIndent();

    ByteSpan span = variableSpan(ic, tree);
    VariableElement variableElement = variableElement(ic, tree);

    QName qName = QNameDescriptor.describe(ic, variableElement, true);
    ic.logger.decreaseIndent();

    Type type = variableType(ic, tree, span);
    List<Modifier> modifiers = variableModifiers(ic, tree);
    List<Annotation> annotations = variableAnnotations(ic, tree);

    if (variableElement.getKind() == ElementKind.FIELD
        || variableElement.getKind() == ElementKind.ENUM
        || variableElement.getKind() == ElementKind.PARAMETER) {
      throw new DescriptorException(
          tree.toString() + " expected local variable found field or parameter");
    }

    LocalDeclarationKey key =
        new LocalDeclarationKey.Builder()
            .setName(qName)
            .setType(type)
            .setModifier(modifiers)
            .setAnnotations(annotations)
            .setSpan(span)
            .setContainer(eContainer)
            .build();

    LocalDeclaration local = new LocalDeclaration.Builder().setKey(key).build();
    ic.predicates.localDeclarationPredicate.addFact(local);
    return local;
  }

  public static ParameterDeclaration describeParameter(IndexerContext ic, VariableTree tree) {
    ic.logger.indentedLog("VariableDeclaration");
    ic.logger.increaseIndent();

    ic.logger.indentedLog("Parameter definition");
    ic.logger.increaseIndent();

    ByteSpan span = variableSpan(ic, tree);
    VariableElement variableElement = variableElement(ic, tree);

    QName qName = QNameDescriptor.describe(ic, variableElement, true);
    ic.logger.decreaseIndent();

    Type type = variableType(ic, tree, span);
    List<Modifier> modifiers = variableModifiers(ic, tree);
    List<Annotation> annotations = variableAnnotations(ic, tree);

    if (variableElement.getKind() == ElementKind.FIELD
        || variableElement.getKind() == ElementKind.ENUM
        || variableElement.getKind() == ElementKind.LOCAL_VARIABLE) {
      throw new DescriptorException(tree.toString() + " expected local variable found field");
    }

    ParameterDeclarationKey key =
        new ParameterDeclarationKey.Builder()
            .setName(qName)
            .setType(type)
            .setModifier(modifiers)
            .setAnnotations(annotations)
            .setSpan(span)
            .build();

    ParameterDeclaration parameter = new ParameterDeclaration.Builder().setKey(key).build();
    return parameter;
  }
}
