// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors.utils;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.sun.source.tree.Tree;
import com.sun.source.util.TreePath;
import com.sun.tools.javac.code.Symbol;
import javax.lang.model.element.Element;

public class ElementUtils {
  public static Element getElement(IndexerContext ic, Tree tree) {
    TreePath treePath = TreePath.getPath(ic.cu, tree);
    return ic.trees.getElement(treePath);
  }

  public static Symbol getSymbol(IndexerContext ic, Element element) {
    if (element instanceof Symbol) {
      return (Symbol) element;
    } else {
      throw new DescriptorException(ic, element, "Cannot cast element to symbol");
    }
  }
}
