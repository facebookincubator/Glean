// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors.utils;

import com.facebook.glean.IndexerContext;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import javax.tools.FileObject;

public class FileLinesUtils {

  public static class FileLinesFields {
    public ArrayList<Long> lineLengths;
    public boolean endsInNewline;
    public boolean hasUnicodeOrTabs;
  }

  public static FileLinesFields getFileLineEndings(IndexerContext ic) {
    FileObject file = ic.cu.getSourceFile();

    CharBuffer content;
    ByteBuffer bytes;

    try {
      content = CharBuffer.wrap(file.getCharContent(false));
      bytes = Charset.forName("UTF-8").encode(content);
      content.rewind();
    } catch (Exception e) {
      content = CharBuffer.wrap(new char[0]);
      bytes = ByteBuffer.wrap(new byte[0]);
    }

    FileLinesFields fields = new FileLinesFields();
    fields.lineLengths = getLineLengths(bytes);
    fields.endsInNewline = getEndsInNewline(content);
    fields.hasUnicodeOrTabs = getHasUnicodeOrTabs(content);
    return fields;
  }

  // length in bytes of each line of file terminated by \n
  private static ArrayList<Long> getLineLengths(ByteBuffer bytes) {
    ArrayList<Long> lineLengths = new ArrayList<>();
    long i = 1;
    while (bytes.hasRemaining()) {
      byte b = bytes.get();
      if (b == '\n') {
        lineLengths.add(i);
        i = 0;
      }
      i++;
    }
    return lineLengths;
  }

  // set flag if last byte is newline
  private static boolean getEndsInNewline(CharBuffer chars) {
    return chars.charAt(chars.length() - 1) == '\n';
  }

  // set flag if non-ascii or tab
  private static boolean getHasUnicodeOrTabs(CharBuffer chars) {
    // set flag if non-ascii or tab
    boolean hasUnicodeOrTabs = false;
    while (chars.hasRemaining()) {
      char c = chars.get();
      if ((int) c > 0x7f || c == '\t') {
        hasUnicodeOrTabs = true;
        break;
      }
    }
    return hasUnicodeOrTabs;
  }
}
