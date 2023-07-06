// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.Utils;
import com.facebook.glean.descriptors.utils.FileLinesUtils;
import com.facebook.glean.schema.src.File;
import com.facebook.glean.schema.src.FileLines;
import com.facebook.glean.schema.src.FileLinesKey;

public class FileLinesDescriptor {

  public static FileLines describe(IndexerContext ic) {

    FileLinesUtils.FileLinesFields fields = FileLinesUtils.getFileLineEndings(ic);

    File file =
        new File.Builder()
            .setKey(Utils.normalizePath(ic.cu.getSourceFile().toUri().getPath()))
            .build();

    FileLinesKey key =
        new FileLinesKey.Builder()
            .setFile(file)
            .setLengths(fields.lineLengths)
            .setEndsInNewline(fields.endsInNewline)
            .setHasUnicodeOrTabs(fields.hasUnicodeOrTabs)
            .build();

    FileLines fileLines = new FileLines.Builder().setKey(key).build();

    return fileLines;
  }
}
