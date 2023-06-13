// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.descriptors.utils.ElementUtils;
import com.facebook.glean.descriptors.utils.SignatureGenerator;
import com.facebook.glean.descriptors.utils.TypeUtils;
import com.facebook.glean.schema.java_alpha.ArrayType;
import com.facebook.glean.schema.java_alpha.ArrayTypeKey;
import com.facebook.glean.schema.java_alpha.BaseType;
import com.facebook.glean.schema.java_alpha.ObjectType;
import com.facebook.glean.schema.java_alpha.Type;
import com.facebook.glean.schema.java_alpha.TypeArg;
import com.facebook.glean.schema.java_alpha.TypeArgKey;
import com.facebook.glean.schema.java_alpha.TypeKey;
import com.facebook.glean.schema.java_alpha.TypeParam;
import com.facebook.glean.schema.java_alpha.TypeParamKey;
import com.facebook.glean.schema.java_alpha.TypeVar;
import com.facebook.glean.schema.java_alpha.TypeVarKey;
import com.facebook.glean.schema.java_alpha.Wildcard;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.ArrayTypeTree;
import com.sun.source.tree.ParameterizedTypeTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.TypeParameterTree;
import com.sun.source.tree.WildcardTree;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.WildcardType;

public class TypeDescriptor {

  private static final List<TypeArg> EMPTY_LIST = Collections.emptyList();

  public static Type describeType(IndexerContext ic, Tree source, TypeMirror typeMirror) {
    return TypeDescriptor.describeType(ic, source, typeMirror, null, true);
  }

  public static Type describeType(
      IndexerContext ic, Tree source, TypeMirror typeMirror, ByteSpan optSpan) {
    return TypeDescriptor.describeType(ic, source, typeMirror, optSpan, true);
  }

  public static TypeArg describeTypeArg(IndexerContext ic, Tree source, TypeMirror typeArgMirror) {
    return TypeDescriptor.describeTypeArg(ic, source, typeArgMirror, true);
  }

  public static TypeParam describeTypeParam(
      IndexerContext ic, TypeParameterTree source, TypeParameterElement typeParameterElement) {
    List<? extends TypeMirror> extendTypes = typeParameterElement.getBounds();
    List<? extends Tree> extendSources = source.getBounds();
    if (extendSources.size() > 0 && extendTypes.size() != extendSources.size()) {
      throw new DescriptorException(
          ic,
          typeParameterElement,
          "source bounds and element bounds disagree for type Parameter " + source.toString());
    }
    int size = extendSources.size();
    List<Type> bounds = new ArrayList<Type>(size);
    for (int i = 0; i < size; i++) {
      bounds.add(
          TypeDescriptor.describeType(ic, extendSources.get(i), extendTypes.get(i), null, false));
    }

    TypeParamKey key =
        new TypeParamKey.Builder()
            .setName(NameDescriptor.describe(ic, typeParameterElement.getSimpleName().toString()))
            .setExtends_(bounds)
            .setSpan(LocationDescriptor.getByteSpanOfTree(ic, source))
            .build();
    TypeParam typeParam = new TypeParam.Builder().setKey(key).build();
    ic.logger.indentedLog("TypeParam: " + TypeDescriptor.toStringTypeParam(typeParam));
    return typeParam;
  }

  public static List<TypeParam> describeTypeParams(
      IndexerContext ic, List<? extends TypeParameterTree> typeParameterSourceTrees) {
    List<TypeParameterElement> typeParameterElements =
        typeParameterSourceTrees.stream()
            .map(typeParameterTree -> ElementUtils.getElement(ic, typeParameterTree))
            .filter(element -> element != null)
            .map(element -> (TypeParameterElement) element)
            .collect(Collectors.toList());
    int size = typeParameterSourceTrees.size();
    if (size != typeParameterElements.size()) {
      throw new DescriptorException(
          "Source type parameter list did not match element list in "
              + typeParameterSourceTrees.toString());
    }
    List<TypeParam> typeParams = new ArrayList<TypeParam>(size);
    for (int i = 0; i < size; i++) {
      typeParams.add(
          TypeDescriptor.describeTypeParam(
              ic, typeParameterSourceTrees.get(i), typeParameterElements.get(i)));
    }
    return typeParams;
  }

  public static List<Type> describeTypes(IndexerContext ic, List<? extends Tree> sources) {
    List<? extends TypeMirror> typeMirrors =
        sources.stream()
            .map(tree -> TypeUtils.GetTypeMirror(ic, tree))
            .filter(typeMirror -> typeMirror != null)
            .collect(Collectors.toList());
    int size = sources.size();
    if (size != typeMirrors.size()) {
      throw new DescriptorException(
          "source of types does not all have type mirrors " + sources.toString());
    }
    List<Type> types = new ArrayList<Type>(size);
    for (int i = 0; i < size; i++) {
      types.add(TypeDescriptor.describeType(ic, sources.get(i), typeMirrors.get(i)));
    }
    return types;
  }

  private static Type describeType(
      IndexerContext ic, Tree source, TypeMirror typeMirror, ByteSpan optSpan, boolean log) {
    Type type = describeTypeWithoutAddingToPredicate(ic, source, typeMirror, optSpan);
    if (log) {
      ic.logger.indentedLog("Type: " + TypeDescriptor.toStringType(type));
    }
    return type;
  }

  private static TypeArg describeTypeArg(
      IndexerContext ic, Tree source, TypeMirror typeArgMirror, boolean log) {
    TypeArg typeArg = null;
    if (typeArgMirror.getKind() == TypeKind.WILDCARD) {
      if (!(source instanceof WildcardTree)) {
        throw new DescriptorException(
            "source not yet needed instance it is as " + source.toString());
      }
      typeArg =
          new TypeArg.Builder()
              .setKey(
                  TypeArgKey.fromWildcard(
                      TypeDescriptor.describeWildcard(
                          ic, (WildcardTree) source, (WildcardType) typeArgMirror)))
              .build();
    } else {
      typeArg =
          new TypeArg.Builder()
              .setKey(
                  TypeArgKey.fromType(
                      TypeDescriptor.describeTypeWithoutAddingToPredicate(
                          ic, source, typeArgMirror, null)))
              .build();
    }

    if (typeArg == null) {
      throw new DescriptorException("Unable to describe typeArg: " + typeArgMirror.toString());
    }

    if (log) {
      ic.logger.indentedLog("TypeArg: " + TypeDescriptor.toStringTypeArg(typeArg));
    }

    return typeArg;
  }

  private static Wildcard describeWildcard(
      IndexerContext ic, WildcardTree source, WildcardType wildcardType) {
    Wildcard wildcard = null;
    if (wildcardType.getExtendsBound() != null) {
      return Wildcard.fromExtends_(
          TypeDescriptor.describeTypeWithoutAddingToPredicate(
              ic, source.getBound(), wildcardType.getExtendsBound(), null));
    } else if (wildcardType.getSuperBound() != null) {
      return Wildcard.fromSuper_(
          TypeDescriptor.describeTypeWithoutAddingToPredicate(
              ic, source.getBound(), wildcardType.getSuperBound(), null));
    } else {
      return Wildcard.fromUnbounded(true);
    }
  }

  private static Type describeTypeWithoutAddingToPredicate(
      IndexerContext ic, Tree source, TypeMirror typeMirror, ByteSpan optSpan) {
    // Consider throwing an error
    if (typeMirror == null) {
      throw new DescriptorException("TypeMirror is null");
    }

    TypeKind typeKind = typeMirror.getKind();
    // Consider throwing an error
    if (typeKind == null) {
      throw new DescriptorException("TypeMirror kind is null");
    }

    if (TypeUtils.IsPrimitive(typeMirror)) {
      String primitive = typeKind.toString().toLowerCase();
      return new Type.Builder()
          .setKey(
              new TypeKey.Builder()
                  .setBaseType(
                      BaseType.fromPrimitive(PrimitiveTypeDescriptor.describe(ic, primitive)))
                  .setTypeArgs(EMPTY_LIST)
                  .setInteropType(SignatureGenerator.assembleForType(ic, typeMirror))
                  .build())
          .build();
    }

    if (typeKind == TypeKind.TYPEVAR) {
      TypeVar typeVariable =
          new TypeVar.Builder()
              .setKey(
                  new TypeVarKey.Builder()
                      .setType(NameDescriptor.describe(ic, typeMirror.toString()))
                      .setSpan(LocationDescriptor.getByteSpanOfTree(ic, source))
                      .build())
              .build();
      return new Type.Builder()
          .setKey(
              new TypeKey.Builder()
                  .setBaseType(BaseType.fromVariable(typeVariable))
                  .setTypeArgs(EMPTY_LIST)
                  .setInteropType(SignatureGenerator.assembleForType(ic, typeMirror))
                  .build())
          .build();
    }

    if (TypeUtils.IsArray(typeMirror)) {
      Tree nextType = null;
      if (source instanceof ArrayTypeTree) {
        ArrayTypeTree arraySource = (ArrayTypeTree) source;
        nextType = arraySource.getType();
      }
      TypeUtils.UnWrappedArray unWrappedArray = TypeUtils.UnWrapArray(typeMirror);
      Type componentType =
          TypeDescriptor.describeTypeWithoutAddingToPredicate(
              ic, nextType, unWrappedArray.componentType, null);
      ArrayType arrayType =
          new ArrayType.Builder()
              .setKey(
                  new ArrayTypeKey.Builder()
                      .setContents(componentType)
                      .setSpan(LocationDescriptor.getByteSpanOfTree(ic, source))
                      .build())
              .build();

      return new Type.Builder()
          .setKey(
              new TypeKey.Builder()
                  .setBaseType(BaseType.fromArray(arrayType))
                  .setTypeArgs(EMPTY_LIST)
                  .setInteropType(SignatureGenerator.assembleForType(ic, typeMirror))
                  .build())
          .build();
    }

    if (typeKind == TypeKind.DECLARED) {
      DeclaredType declaredType = (DeclaredType) typeMirror;
      TypeElement declaredTypeElement = (TypeElement) declaredType.asElement();
      String type =
          ElementUtils.getSymbol(ic, declaredType.asElement()).getQualifiedName().toString();
      if (source instanceof ParameterizedTypeTree) {
        ParameterizedTypeTree complexTypeTree = (ParameterizedTypeTree) source;
        Tree baseType = complexTypeTree.getType();
        List<? extends Tree> typeArgsTrees = complexTypeTree.getTypeArguments();
        List<? extends TypeMirror> typeArgsMirrors = declaredType.getTypeArguments();
        if (typeArgsTrees.size() != typeArgsMirrors.size()) {
          throw new DescriptorException(
              ic, declaredTypeElement, "Declared and checked parameters not same size");
        }
        ArrayList<TypeArg> typeArgs = new ArrayList<>(typeArgsTrees.size());
        for (int i = 0; i < typeArgsTrees.size(); i++) {
          typeArgs.add(
              TypeDescriptor.describeTypeArg(
                  ic, typeArgsTrees.get(i), typeArgsMirrors.get(i), true));
        }
        ObjectType dType =
            ObjectTypeDescriptor.describe(ic, baseType, declaredTypeElement, optSpan);
        return new Type.Builder()
            .setKey(
                new TypeKey.Builder()
                    .setTypeArgs(typeArgs)
                    .setBaseType(BaseType.fromObject(dType))
                    .setInteropType(
                        SignatureGenerator.assembleForType(ic, declaredTypeElement.asType()))
                    .build())
            .build();
      } else {
        // Type is flat structure
        return new Type.Builder()
            .setKey(
                new TypeKey.Builder()
                    .setTypeArgs(new ArrayList<>())
                    .setBaseType(
                        BaseType.fromObject(
                            ObjectTypeDescriptor.describe(
                                ic, source, declaredTypeElement, optSpan)))
                    .setInteropType(
                        SignatureGenerator.assembleForType(ic, declaredTypeElement.asType()))
                    .build())
            .build();
      }
    }

    throw new DescriptorException(
        "Cannot get type: " + typeMirror.toString() + ":" + typeKind.toString());
  }

  private static String toStringType(Type type) {
    TypeKey typeKey = type.getKey();
    List<TypeArg> typeArgs = typeKey.getTypeArgs();
    // Todo may need to build some better string getters for these
    String typeString = typeKey.getBaseType().toString();

    if (typeArgs != null && typeArgs.size() > 0) {
      String typeArgsString =
          String.join(
              ", ",
              typeArgs.stream()
                  .map(typeArg -> TypeDescriptor.toStringTypeArg(typeArg))
                  .collect(Collectors.toList()));
      typeString += "<" + typeArgsString + ">";
    }

    return typeString;
  }

  private static String toStringTypeArg(TypeArg typeArg) {
    TypeArgKey typeArgKey = typeArg.getKey();
    if (typeArgKey.isSetWildcard()) {
      return TypeDescriptor.toStringWildcard(typeArgKey.getWildcard());
    } else if (typeArgKey.isSetType()) {
      return TypeDescriptor.toStringType(typeArgKey.getType());
    }

    throw new RuntimeException("Cannot toString typeArg: " + typeArg.toString());
  }

  private static String toStringWildcard(Wildcard wildcard) {
    if (wildcard.isSetExtends_()) {
      return "+" + TypeDescriptor.toStringType(wildcard.getExtends_());
    } else if (wildcard.isSetSuper_()) {
      return "-" + TypeDescriptor.toStringType(wildcard.getSuper_());
    } else if (wildcard.isSetUnbounded()) {
      return "?";
    }

    throw new RuntimeException("Cannot toString wildcard: " + wildcard.toString());
  }

  private static String toStringTypeParam(TypeParam typeParam) {
    TypeParamKey typeParamKey = typeParam.getKey();
    List<Type> bounds = typeParamKey.getExtends_();
    String typeParamString = typeParamKey.getName().getKey();
    if (bounds != null && !bounds.isEmpty()) {
      typeParamString +=
          " extends "
              + String.join(
                  "& ",
                  bounds.stream()
                      .map(type -> TypeDescriptor.toStringType(type))
                      .collect(Collectors.toList()));
    }
    return typeParamString;
  }
}
