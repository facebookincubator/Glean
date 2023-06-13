// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors.utils;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.PathDescriptor;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.schema.javakotlin_alpha.Path;
import com.facebook.glean.schema.javakotlin_alpha.Type;
import com.facebook.glean.schema.javakotlin_alpha.TypeKey;
import com.sun.tools.javac.code.Symbol;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.WildcardType;

// Inspired by com.sun.tools.javac.code.Types.SignatureGenerator
// https://java-browser.yawk.at/java/14/jdk.compiler/com/sun/tools/javac/code/Types.java#com.sun.tools.javac.code.Types.SignatureGenerator

/**
 * Generates a compact unique identifier for a type. These can be used to distinguish things with
 * the same name but different types.
 */
public class SignatureGenerator {
  private static Type assembleForClass(IndexerContext ic, DeclaredType declaredType) {
    Element declaredTpeElement = declaredType.asElement();
    if (declaredTpeElement == null) {
      throw new DescriptorException(declaredType.toString() + " did not have an element");
    }
    Symbol declaredTypeSymbol = ElementUtils.getSymbol(ic, declaredTpeElement);
    String qualifiedName = declaredTypeSymbol.getQualifiedName().toString();
    Path path = PathDescriptor.describe(ic, qualifiedName);
    return new Type.Builder().setKey(TypeKey.fromObject(path)).build();
  }

  private static Type assembleJavaLangObject(IndexerContext ic) {
    PathDescriptor.NameAndPath namePath = PathDescriptor.structurePath(ic, "java.lang.Object");
    Path path = PathDescriptor.describe(ic, namePath.simple, namePath.path);
    return new Type.Builder().setKey(TypeKey.fromObject(path)).build();
  }

  private static Type assemblePrimitive(String primitive) {
    return new Type.Builder().setKey(TypeKey.fromPrimitive(primitive)).build();
  }

  public static List<Type> assembleForTypes(
      IndexerContext ic, List<? extends TypeMirror> typeMirrors) {
    return typeMirrors.stream()
        .map(typeMirror -> assembleForType(ic, typeMirror))
        .collect(Collectors.toList());
  }

  public static Type assembleForType(IndexerContext ic, TypeMirror typeMirror) {
    switch (typeMirror.getKind()) {
      case BOOLEAN:
        return assemblePrimitive("Z");
      case BYTE:
        return assemblePrimitive("B");
      case CHAR:
        return assemblePrimitive("C");
      case DOUBLE:
        return assemblePrimitive("D");
      case FLOAT:
        return assemblePrimitive("F");
      case INT:
        return assemblePrimitive("I");
      case LONG:
        return assemblePrimitive("J");
      case SHORT:
        return assemblePrimitive("S");
      case VOID:
        return assemblePrimitive("V");
      case DECLARED:
        return assembleForClass(ic, (DeclaredType) typeMirror);
      case ARRAY:
        return new Type.Builder()
            .setKey(
                TypeKey.fromArray(assembleForType(ic, ((ArrayType) typeMirror).getComponentType())))
            .build();
      case EXECUTABLE:
        {
          throw new DescriptorException("SignatureGenerator unexpectedly received Executable type");
        }
      case WILDCARD:
        {
          WildcardType wildcardType = (WildcardType) typeMirror;
          if (wildcardType.getExtendsBound() != null) {
            return assembleForType(ic, wildcardType.getExtendsBound());
          } else if (wildcardType.getSuperBound() != null) {
            return assembleForType(ic, wildcardType.getSuperBound());
          } else {
            // unbounded
            return assembleJavaLangObject(ic);
          }
        }
      case TYPEVAR:
        return assembleJavaLangObject(ic);
      case ERROR:
        // Error represents a class or interface type that could not be resolved.
        // In these cases resort to Object which is the superclass of all classes / interfaces.
        return assembleJavaLangObject(ic);
      default:
        throw new DescriptorException(
            "Cannot assemble signature for TypeKind: " + typeMirror.getKind().toString());
    }
  }
}
