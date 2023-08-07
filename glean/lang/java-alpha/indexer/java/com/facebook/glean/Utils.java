// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

// @file:Suppress("JAVA_MODULE_DOES_NOT_EXPORT_PACKAGE")

package com.facebook.glean;

import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.Trees;
import com.sun.tools.javac.parser.JavaTokenizer;
import com.sun.tools.javac.parser.Scanner;
import com.sun.tools.javac.parser.ScannerFactory;
import com.sun.tools.javac.parser.Tokens.Comment;
import com.sun.tools.javac.parser.Tokens.Comment.CommentStyle;
import com.sun.tools.javac.parser.Tokens.Token;
import com.sun.tools.javac.parser.Tokens.TokenKind;
import com.sun.tools.javac.parser.UnicodeReader;
import com.sun.tools.javac.util.Context;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.*;

public class Utils {
  // e.g. to match 'fbsource/147-16788900/fbandroid/*'
  private static final Pattern NOISE = Pattern.compile("^[0-9]+-[0-9]+/");

  // to match any remnant repo anchors
  // e.g.
  // '/data/sandcastle/boxes/eden-trunk-hg-fbcode-fbsource/''
  // or
  // '/data/sandcastle/boxes/fbsource/'
  //
  private static final Pattern FBSOURCE = Pattern.compile("^/.*[/-]fbsource/");

  // what we need is to pass an indexerRoot through...

  /** Input paths are absolute, and need to be made relative and de-noised */
  public static String normalizePath(String path) {
    Matcher n = FBSOURCE.matcher(path);
    String cleanPath = n.replaceFirst("");
    Matcher m = NOISE.matcher(cleanPath);
    return m.replaceFirst("");
  }

  public static class Lexer {
    private String mSource;
    private CompilationUnitTree mUnit;
    private Trees mTrees;
    private List<Token> tokens;
    private List<Token> reversedTokens;

    public Lexer(String source, CompilationUnitTree unit, Trees trees) {
      mSource = source;
      mUnit = unit;
      mTrees = trees;
      tokens = tokenize();
      reversedTokens = new ArrayList<Token>(tokens);
      Collections.reverse(reversedTokens);
    }

    public Token findNextToken(int end, TokenKind kind) {
      assert (end >= 0 && end < mSource.length());

      for (Token token : tokens) {
        if (token.pos >= end && token.kind == kind) return token;
        if (token.pos >= end) return null;
      }
      return null;
    }

    public Token findNextToken(Tree tree, TokenKind kind) {
      return findNextToken(endPosition(tree), kind);
    }

    public Token findFirstTokenOf(Tree tree) {
      return findFirstTokenOf(startPosition(tree));
    }

    private Token findFirstTokenOf(int start) {
      assert (start > 0 && start < mSource.length());
      for (Token token : tokens) {
        if (token.pos == start) return token;
        if (token.pos > start) return null;
      }
      return null;
    }

    public Token findEndTokenOf(Tree tree) {
      return findEndTokenOf(endPosition(tree));
    }

    private Token findEndTokenOf(int endPos) {
      assert (endPos > 0 && endPos < mSource.length());
      for (Token token : reversedTokens) {
        if (token.endPos == endPos) return token;
        if (token.endPos < endPos) return null;
      }
      return null;
    }

    private Token findPreviousToken(int start, TokenKind kind) {
      assert (start > 0 && start < mSource.length());

      for (Token token : reversedTokens) {
        if (token.pos < start && token.kind == kind) return token;
        if (token.pos < start) return null;
      }
      return null;
    }

    public Token findPreviousToken(Tree tree, TokenKind kind) {
      return findPreviousToken(startPosition(tree), kind);
    }

    public int startPosition(Tree tree) {
      return (int) mTrees.getSourcePositions().getStartPosition(mUnit, tree);
    }

    public int endPosition(Tree tree) {
      return (int) mTrees.getSourcePositions().getEndPosition(mUnit, tree);
    }

    private List<Token> tokenize() {
      Context context = new Context();
      ScannerFactory factory = ScannerFactory.instance(context);
      char[] buffer = mSource.toCharArray();
      JavaTokenizer tokenizer =
          new AccessibleTokenizer(factory, new AccessibleReader(factory, buffer, buffer.length));
      AccessibleScanner scanner = new AccessibleScanner(factory, tokenizer);
      List<Token> tokens = new ArrayList<Token>();
      do {
        scanner.nextToken();
        Token t = scanner.token();
        tokens.add(t);
      } while (scanner.token().kind != TokenKind.EOF);

      return tokens;
    }
  }

  private static class AccessibleTokenizer extends JavaTokenizer {
    AccessibleTokenizer(ScannerFactory factory, UnicodeReader reader) {
      super(factory, reader);
    }

    public Comment processComment(int pos, int endPos, CommentStyle style) {
      char[] buf = reader.getRawCharacters(pos, endPos);
      return new InternalComment(pos, endPos, new AccessibleReader(fac, buf, buf.length), style);
    }
  }

  private static class AccessibleReader extends UnicodeReader {
    AccessibleReader(ScannerFactory factory, char[] buffer, int bufferLen) {
      super(factory, buffer, bufferLen);
    }
  }

  private static class AccessibleScanner extends Scanner {
    AccessibleScanner(ScannerFactory factory, JavaTokenizer tokenizer) {
      super(factory, tokenizer);
    }
  }

  private static class InternalComment implements Comment {

    int pos;
    int endPos;
    private AccessibleReader reader;
    private CommentStyle style;
    private String txt;

    InternalComment(int pos, int endPos, AccessibleReader reader, CommentStyle style) {
      this.pos = pos;
      this.endPos = endPos;
      this.reader = reader;
      this.style = style;
      this.txt = new String(reader.getRawCharacters());
    }

    public int getSourcePos(int index) {
      assert (0 <= index && index < endPos - pos);
      return pos + index;
    }

    public CommentStyle getStyle() {
      return style;
    }

    public String getText() {
      return txt;
    }

    public boolean isDeprecated() {
      return false;
    }

    public String toString() {
      return txt;
    }
  }
}
