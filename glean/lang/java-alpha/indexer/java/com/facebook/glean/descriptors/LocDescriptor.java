// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.Utils;
import com.facebook.glean.schema.src.File;
import com.facebook.glean.schema.src.Loc;
import com.sun.source.tree.Tree;

public class LocDescriptor {
  public static Loc describe(IndexerContext ic, Tree tree) {
    long startPosition = ic.sourcePositions.getStartPosition(ic.cu, tree);
    int lineNumber = (int) ic.lineMap.getLineNumber(startPosition);
    int columnNumber = (int) ic.lineMap.getColumnNumber(startPosition);
    return LocDescriptor.describe(
        ic.cu.getSourceFile().toUri().getPath(), lineNumber, columnNumber);
  }

  private static Loc describe(String file, int line, int column) {
    Loc loc =
        new Loc.Builder()
            .setFile(new File.Builder().setKey(Utils.normalizePath(file)).build())
            .setLine(line)
            .setColumn(column)
            .build();

    return loc;
  }
}
