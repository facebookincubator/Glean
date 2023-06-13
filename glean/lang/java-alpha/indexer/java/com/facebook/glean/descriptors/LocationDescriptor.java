// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.Utils;
import com.facebook.glean.schema.src.ByteSpan;
import com.facebook.glean.schema.src.File;
import com.facebook.glean.schema.src.FileLocation;
import com.sun.source.tree.Tree;

public class LocationDescriptor {

  public static ByteSpan getByteSpanOfTree(IndexerContext ic, Tree tree) {
    long startPosition = ic.sourcePositions.getStartPosition(ic.cu, tree);
    long endPosition = ic.sourcePositions.getEndPosition(ic.cu, tree);
    long length = endPosition - startPosition;
    ic.logger.indentedLog("ByteSpan: " + startPosition + " " + " " + endPosition + " " + length);
    return new ByteSpan.Builder().setStart(startPosition).setLength(length).build();
  }

  public static FileLocation describe(IndexerContext ic, Tree tree) {
    ByteSpan span = getByteSpanOfTree(ic, tree);
    File file =
        new File.Builder()
            .setKey(Utils.normalizePath(ic.cu.getSourceFile().toUri().getPath()))
            .build();
    FileLocation fileLocation = new FileLocation.Builder().setSpan(span).setFile(file).build();
    return fileLocation;
  }
}
