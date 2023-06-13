// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.Modifier;
import com.sun.source.tree.ModifiersTree;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

// https://docs.oracle.com/javase/8/docs/api/javax/lang/model/element/Modifier.html

public class ModifierDescriptor {
  public static List<Modifier> describe(IndexerContext ic, ModifiersTree modifiersTree) {
    List<Modifier> modifierList =
        modifiersTree.getFlags().stream()
            .map(javaxModifier -> convertToGleanModifier(javaxModifier))
            .collect(Collectors.toList());

    Collections.sort(modifierList);
    ic.logger.indentedLog("Modifiers: " + modifierList.toString());
    return modifierList;
  }

  private static Modifier convertToGleanModifier(javax.lang.model.element.Modifier javaxModifier) {
    switch (javaxModifier) {
      case ABSTRACT:
        return Modifier.ABSTRACT_;
      case DEFAULT:
        return Modifier.DEFAULT_;
      case FINAL:
        return Modifier.FINAL_;
      case NATIVE:
        return Modifier.NATIVE_;
      case PRIVATE:
        return Modifier.PRIVATE_;
      case PROTECTED:
        return Modifier.PROTECTED_;
      case PUBLIC:
        return Modifier.PUBLIC_;
      case STATIC:
        return Modifier.STATIC_;
      case STRICTFP:
        return Modifier.STRICTFP_;
      case SYNCHRONIZED:
        return Modifier.SYNCHRONIZED_;
      case TRANSIENT:
        return Modifier.TRANSIENT_;
      case VOLATILE:
        return Modifier.VOLATILE_;
      default:
        throw new RuntimeException("Unknown modifier: " + javaxModifier.toString());
    }
  }
}
