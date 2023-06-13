// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors.utils;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.Utils;
import com.sun.source.tree.Tree;
import javax.lang.model.element.Element;

public class LocUtils {
  public static String getLocationStringOfTree(IndexerContext ic, Tree tree) {
    long startPosition = ic.sourcePositions.getStartPosition(ic.cu, tree);
    int lineNumber = (int) ic.lineMap.getLineNumber(startPosition);
    int columnNumber = (int) ic.lineMap.getColumnNumber(startPosition);
    String path = Utils.normalizePath(ic.cu.getSourceFile().toUri().getPath());
    return path + ":" + lineNumber + "." + columnNumber + "  " + tree.getKind().toString();
  }

  public static String getLocationStringOfElement(IndexerContext ic, Element element) {
    try {
      return LocUtils.getLocationStringOfTree(ic, ic.trees.getTree(element));
    } catch (Exception e) {
      return "";
    }
  }
}
