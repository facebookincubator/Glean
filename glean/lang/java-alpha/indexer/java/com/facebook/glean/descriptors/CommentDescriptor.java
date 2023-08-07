// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.Declaration;
import com.facebook.glean.schema.java_alpha.DeclarationComment;
import com.facebook.glean.schema.java_alpha.DeclarationCommentKey;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.Tree;
import com.sun.tools.javac.parser.Tokens.Comment;
import com.sun.tools.javac.parser.Tokens.Token;
import java.util.List;

public class CommentDescriptor {
  public static DeclarationComment describe(IndexerContext ic, Tree tree, Declaration decl) {
    ic.logger.indentedLog("CommentDescriptor");

    if (ic.lexer == null) {
      return null;
    }
    Token potentiallyWithCommentToken = ic.lexer.findFirstTokenOf(tree);
    if (potentiallyWithCommentToken == null) {
      return null;
    }

    List<Comment> potentialComments = potentiallyWithCommentToken.comments;
    if (potentialComments == null || potentialComments.isEmpty()) {
      return null;
    }

    Comment closest = potentialComments.get(0);
    int commentSourceStart = closest.getSourcePos(0);
    String commentText = closest.getText();

    DeclarationCommentKey commentKey =
        new DeclarationCommentKey.Builder()
            .setSpan(new ByteSpan(commentSourceStart, commentSourceStart + commentText.length()))
            .setFile(ClassUtils.buildFile(ic))
            .setDeclaration(decl)
            .build();
    DeclarationComment c = new DeclarationComment.Builder().setKey(commentKey).build();
    ic.predicates.declarationCommentPredicate.addFact(c);
    return c;
  }
}
