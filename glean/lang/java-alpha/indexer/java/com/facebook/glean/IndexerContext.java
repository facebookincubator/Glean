// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean;

import com.facebook.glean.logger.BufferedLogger;
import com.facebook.glean.logger.Logger;
import com.facebook.glean.predicates.Predicates;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.LineMap;
import com.sun.source.tree.Tree;
import com.sun.source.util.JavacTask;
import com.sun.source.util.SourcePositions;
import com.sun.source.util.Trees;
import java.io.IOException;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;

public class IndexerContext {
  public final int ID = IndexerContext.getNextID();

  public final JavacTask task;
  public final Elements elements;
  public final Trees trees;
  public final Types types;

  public final CompilationUnitTree cu;
  public final Utils.Lexer lexer;
  public final SourcePositions sourcePositions;
  public final LineMap lineMap;

  public final Predicates predicates = new Predicates();

  public final boolean loggingEnabled;
  public final Logger logger = new Logger();
  public final BufferedLogger bufferedLogger;

  private static int _AutoIncrementID = 0;

  private static synchronized int getNextID() {
    int x = _AutoIncrementID;
    _AutoIncrementID++;
    return x;
  }

  public IndexerContext(JavacTask task, CompilationUnitTree cu, boolean loggingEnabled) {
    this.task = task;
    this.elements = task.getElements();
    this.trees = Trees.instance(task);
    this.types = task.getTypes();

    this.cu = cu;
    try {
      this.lexer = new Utils.Lexer(cu.getSourceFile().getCharContent(false).toString(), cu, trees);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    this.sourcePositions = this.trees.getSourcePositions();
    this.lineMap = cu.getLineMap();

    this.loggingEnabled = loggingEnabled;
    this.bufferedLogger = this.loggingEnabled ? new BufferedLogger() : null;
    if (this.loggingEnabled) {
      logger.setEnabled(true);
      logger.addLogger(this.bufferedLogger);
    }
  }

  public boolean isCompilerGenerated(Tree tree) {
    return -1 == this.sourcePositions.getEndPosition(this.cu, tree);
  }
}
