// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.descriptors.utils.TypeUtils;
import com.facebook.glean.schema.java_alpha.Annotation;
import com.facebook.glean.schema.java_alpha.ConstructorDeclaration;
import com.facebook.glean.schema.java_alpha.Definition;
import com.facebook.glean.schema.java_alpha.FieldDeclaration;
import com.facebook.glean.schema.java_alpha.MethodDeclaration;
import com.facebook.glean.schema.java_alpha.Modifier;
import com.facebook.glean.schema.java_alpha.Type;
import com.facebook.glean.schema.java_alpha.TypeParam;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.ModifiersTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.VariableTree;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;

public class ClassUtils {

  private static Element getElement(
      IndexerContext ic, String source, Tree pathBase, boolean strict) {
    Element element = ic.trees.getElement(ic.trees.getPath(ic.cu, pathBase));
    if (element == null && strict) {
      throw new DescriptorException(source + " did not have viable element");
    }
    return element;
  }

  public static QName buildName(IndexerContext ic, ClassTree tree) {
    Element classElement = getElement(ic, tree.getSimpleName().toString(), tree, true);

    ic.logger.indentedLog("Name");
    ic.logger.increaseIndent();
    QName qName = QNameDescriptor.describe(ic, (TypeElement) classElement, true);
    ic.logger.decreaseIndent();

    return qName;
  }

  public static List<Modifier> buildModifiers(IndexerContext ic, ClassTree tree) {
    ModifiersTree modifiersTree = tree.getModifiers();
    ic.logger.indentedLog("Modifiers");
    ic.logger.increaseIndent();
    List<Modifier> modifiers = ModifierDescriptor.describe(ic, modifiersTree);
    ic.logger.decreaseIndent();
    return modifiers;
  }

  public static List<Annotation> buildAnnotations(IndexerContext ic, ClassTree tree) {
    ic.logger.indentedLog("Annotations");
    ic.logger.increaseIndent();
    List<Annotation> annotations =
        tree.getModifiers().getAnnotations().stream()
            .map(annotationTree -> AnnotationDescriptor.describe(ic, annotationTree))
            .collect(Collectors.toList());
    ic.logger.decreaseIndent();
    return annotations;
  }

  public static Type buildExtends(IndexerContext ic, ClassTree tree) {
    ic.logger.indentedLog("Extends");
    ic.logger.increaseIndent();
    Tree extendsClause = tree.getExtendsClause();
    if (extendsClause == null || ic.isCompilerGenerated(extendsClause)) {
      ic.logger.decreaseIndent();
      return null;
    } else {
      Type extends_ =
          TypeDescriptor.describeType(
              ic, extendsClause, TypeUtils.GetTypeMirror(ic, extendsClause));
      ic.logger.decreaseIndent();
      return extends_;
    }
  }

  public static List<Type> buildImplements(IndexerContext ic, ClassTree tree) {
    ic.logger.indentedLog("Implements");
    ic.logger.increaseIndent();
    List<Type> implements_ =
        tree.getImplementsClause() == null
            ? Collections.emptyList()
            : tree.getImplementsClause().stream()
                .filter(t -> t != null && !(ic.isCompilerGenerated(t)))
                .map(
                    elTree ->
                        TypeDescriptor.describeType(
                            ic, elTree, TypeUtils.GetTypeMirror(ic, elTree)))
                .collect(Collectors.toList());
    ic.logger.decreaseIndent();
    return implements_;
  }

  private static boolean checkKind(Element element, ElementKind kind) {
    return !(element == null) && (element.getKind() == kind);
  }

  private static List<Tree> getMembersOfKind(IndexerContext ic, ClassTree tree, ElementKind kind) {
    return tree.getMembers().stream()
        .filter(el -> checkKind(getElement(ic, el.toString(), el, false), kind))
        .collect(Collectors.toList());
  }

  public static List<FieldDeclaration> buildFields(
      IndexerContext ic, ClassTree tree, Definition container, boolean allowEnums) {
    ic.logger.indentedLog("Fields");
    ic.logger.increaseIndent();
    List<FieldDeclaration> variables =
        getMembersOfKind(ic, tree, ElementKind.FIELD).stream()
            .filter(t -> !ic.isCompilerGenerated(t))
            .map(
                variableTree ->
                    VariableDescriptor.describeField(ic, (VariableTree) variableTree, container))
            .collect(Collectors.toList());
    if (allowEnums) {
      List<FieldDeclaration> enumConstants =
          getMembersOfKind(ic, tree, ElementKind.ENUM_CONSTANT).stream()
              .map(
                  variableTree ->
                      VariableDescriptor.describeField(ic, (VariableTree) variableTree, container))
              .collect(Collectors.toList());
      variables.addAll(enumConstants);
    }
    ic.logger.decreaseIndent();
    return variables;
  }

  public static List<ConstructorDeclaration> buildConstructors(
      IndexerContext ic, ClassTree tree, Definition container) {
    ic.logger.indentedLog("Constructors");
    ic.logger.increaseIndent();
    List<ConstructorDeclaration> constructors =
        getMembersOfKind(ic, tree, ElementKind.CONSTRUCTOR).stream()
            .filter(t -> !ic.isCompilerGenerated(t))
            .map(
                methodTree ->
                    ConstructorDescriptor.describe(ic, (MethodTree) methodTree, container))
            .collect(Collectors.toList());
    ic.logger.decreaseIndent();
    return constructors;
  }

  public static List<MethodDeclaration> buildMethods(
      IndexerContext ic, ClassTree tree, Definition container) {
    ic.logger.indentedLog("Methods");
    ic.logger.increaseIndent();
    List<MethodDeclaration> methods =
        getMembersOfKind(ic, tree, ElementKind.METHOD).stream()
            .filter(t -> !ic.isCompilerGenerated(t))
            .map(methodTree -> MethodDescriptor.describe(ic, (MethodTree) methodTree, container))
            .collect(Collectors.toList());
    ic.logger.decreaseIndent();
    return methods;
  }

  private abstract static class DefinitionBuilder {
    abstract Definition build(IndexerContext ic, ClassTree def);
  }

  private static void updater(
      IndexerContext ic,
      List<Definition> definitions,
      List<Tree> innerList,
      Map<String, Definition> existingMap,
      DefinitionBuilder updater) {
    for (int i = 0; i < innerList.size(); i++) {
      ClassTree innerDef = (ClassTree) innerList.get(i);
      TypeElement classElement =
          (TypeElement) getElement(ic, innerDef.getSimpleName().toString(), innerDef, true);
      String qualifiedName = classElement.getQualifiedName().toString();
      if (existingMap.containsKey(qualifiedName)) {
        definitions.add(existingMap.get(qualifiedName));
      } else {
        Definition d = updater.build(ic, innerDef);
        definitions.add(d);
        existingMap.put(qualifiedName, d);
      }
    }
  }

  public static List<Definition> buildInnerDefinitions(
      IndexerContext ic,
      ClassTree tree,
      Definition container,
      Map<String, Definition> existingDefinitionMap) {
    ic.logger.indentedLog("Inner Definitions");
    ic.logger.increaseIndent();
    List<Tree> innerClassTrees = getMembersOfKind(ic, tree, ElementKind.CLASS);
    List<Tree> innerInterfaceTrees = getMembersOfKind(ic, tree, ElementKind.INTERFACE);
    List<Tree> innerEnumTrees = getMembersOfKind(ic, tree, ElementKind.ENUM);

    List<Definition> definitions =
        new ArrayList<Definition>(
            innerClassTrees.size() + innerInterfaceTrees.size() + innerEnumTrees.size());

    updater(
        ic,
        definitions,
        innerClassTrees,
        existingDefinitionMap,
        new DefinitionBuilder() {
          Definition build(IndexerContext ic, ClassTree def) {
            return Definition.fromClass_(
                ClassDescriptor.describe(ic, def, container, existingDefinitionMap));
          }
        });

    updater(
        ic,
        definitions,
        innerInterfaceTrees,
        existingDefinitionMap,
        new DefinitionBuilder() {
          Definition build(IndexerContext ic, ClassTree def) {
            return Definition.fromInterface_(
                InterfaceDescriptor.describe(ic, def, container, existingDefinitionMap));
          }
        });

    updater(
        ic,
        definitions,
        innerEnumTrees,
        existingDefinitionMap,
        new DefinitionBuilder() {
          Definition build(IndexerContext ic, ClassTree def) {
            return Definition.fromEnum_(
                EnumDescriptor.describe(ic, def, container, existingDefinitionMap));
          }
        });

    ic.logger.decreaseIndent();
    return definitions;
  }

  public static List<TypeParam> buildTypeParams(IndexerContext ic, ClassTree tree) {
    ic.logger.indentedLog("TypeParams");
    ic.logger.increaseIndent();
    List<TypeParam> typeParams = TypeDescriptor.describeTypeParams(ic, tree.getTypeParameters());
    ic.logger.decreaseIndent();
    return typeParams;
  }
}
