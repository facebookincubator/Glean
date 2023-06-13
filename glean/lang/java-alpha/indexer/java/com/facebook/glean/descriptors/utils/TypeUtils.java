// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors.utils;

import com.facebook.glean.IndexerContext;
import com.sun.source.tree.Tree;
import com.sun.source.util.TreePath;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;

public class TypeUtils {

  public static TypeMirror GetTypeMirror(IndexerContext ic, Tree tree) {
    TreePath treePath = TreePath.getPath(ic.cu, tree);
    return ic.trees.getTypeMirror(treePath);
  }

  public static boolean IsPrimitive(TypeMirror typeMirror) {
    TypeKind typeKind = typeMirror.getKind();
    return typeKind.isPrimitive() || typeKind.equals(TypeKind.VOID);
  }

  public static boolean IsArray(TypeMirror typeMirror) {
    return typeMirror.getKind() == TypeKind.ARRAY;
  }

  public static UnWrappedArray UnWrapArray(TypeMirror rootTypeMirror) {
    int depth = 0;
    TypeMirror componentType = rootTypeMirror;
    while (componentType.getKind() == TypeKind.ARRAY) {
      componentType = ((ArrayType) componentType).getComponentType();
      depth++;
    }

    return new UnWrappedArray(depth, componentType);
  }

  public static class UnWrappedArray {
    public final int depth;
    public final TypeMirror componentType;

    UnWrappedArray(int depth, TypeMirror componentType) {
      this.depth = depth;
      this.componentType = componentType;
    }
  }
}
