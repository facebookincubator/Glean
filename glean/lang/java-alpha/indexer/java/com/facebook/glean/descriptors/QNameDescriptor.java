// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import static com.facebook.glean.descriptors.PathDescriptor.NameAndPath;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.debug.DebugUtils;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.descriptors.utils.ElementUtils;
import com.facebook.glean.schema.javakotlin_alpha.Name;
import com.facebook.glean.schema.javakotlin_alpha.Path;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.javakotlin_alpha.QNameKey;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;

public class QNameDescriptor {

  private static QName describeType(
      IndexerContext ic, TypeElement element, boolean addToPredicate) {
    if (element.asType() == null) {
      throw new DescriptorException(ic, element, "element did not have a type");
    }

    NameAndPath expandedName = PathDescriptor.structureTypeName(ic, element);

    QName qName = QNameDescriptor.buildQName(expandedName.simple, expandedName.path);
    if (addToPredicate) {
      QNameDescriptor.addToPredicate(ic, qName);
    }

    return qName;
  }

  public static QName describe(IndexerContext ic, TypeElement typeElement, boolean addToPredicate) {
    return describeType(ic, typeElement, addToPredicate);
  }

  // A helper for building the QName for MethodName, does not build the signatures
  public static QName describe(
      IndexerContext ic, ExecutableElement executableElement, boolean addToPredicate) {
    // Executable elements will always be enclosed in a class
    Element enclosingElement = executableElement.getEnclosingElement();
    if (enclosingElement == null) {
      throw new DescriptorException(
          ic, executableElement, "Null enclosing element for executable element");
    }

    TypeElement enclosingTypeElement = (TypeElement) enclosingElement;
    NameAndPath enclosingTypeNameAndPath =
        PathDescriptor.structureTypeName(ic, enclosingTypeElement);
    Path path =
        PathDescriptor.describe(ic, enclosingTypeNameAndPath.simple, enclosingTypeNameAndPath.path);

    String methodName = executableElement.getSimpleName().toString();
    Name name = NameDescriptor.describe(ic, methodName);

    QName qName = QNameDescriptor.buildQName(name, path);
    if (addToPredicate) {
      QNameDescriptor.addToPredicate(ic, qName);
    }

    return qName;
  }

  public static QName describe(
      IndexerContext ic, VariableElement variableElement, boolean addToPredicate) {
    Element enclosingElement = variableElement.getEnclosingElement();
    if (enclosingElement == null) {
      throw new DescriptorException(ic, variableElement, "Null enclosing element for variable");
    }

    String name = variableElement.getSimpleName().toString();
    if ((variableElement.getKind() == ElementKind.FIELD
            || variableElement.getKind() == ElementKind.ENUM_CONSTANT)
        && !(enclosingElement instanceof TypeElement)) {
      throw new DescriptorException(
          ic, enclosingElement, "enclosingElement for non-local variable not a TypeElement");
    }
    String fqn =
        (variableElement.getKind() == ElementKind.FIELD
                || variableElement.getKind() == ElementKind.ENUM_CONSTANT)
            ? QNameDescriptor.getClassFQN(ic, (TypeElement) enclosingElement) + '.' + name
            : null;
    Path path = null;
    if (fqn != null) {
      NameAndPath enclosingTypeNameAndPath =
          PathDescriptor.structureTypeName(ic, (TypeElement) enclosingElement);
      path =
          PathDescriptor.describe(
              ic, enclosingTypeNameAndPath.simple, enclosingTypeNameAndPath.path);
    }

    QName qName = QNameDescriptor.buildQName(NameDescriptor.describe(ic, name), path);
    if (addToPredicate) {
      QNameDescriptor.addToPredicate(ic, qName);
    }

    return qName;
  }

  private static String getClassFQN(IndexerContext ic, TypeElement typeElement) {
    return ElementUtils.getSymbol(ic, typeElement).getQualifiedName().toString();
  }

  private static QName buildQName(Name name, Path path) {
    QNameKey key = new QNameKey.Builder().setName(name).setContext(path).build();
    return new QName.Builder().setKey(key).build();
  }

  public static void addToPredicate(IndexerContext ic, QName qName) {
    DebugUtils.debugLogQName(ic, qName);
  }
}
