// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors.debug;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.java_alpha.FileXRefs;
import com.facebook.glean.schema.java_alpha.FileXRefsKey;
import com.facebook.glean.schema.java_alpha.XRefKey;
import com.facebook.glean.schema.javakotlin_alpha.MethodName;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.src.ByteSpan;
import java.util.List;
import java.util.stream.Collectors;

public class DebugUtils {
  public static void debugLogFileXrefs(IndexerContext ic, FileXRefs fileXRefs) {
    FileXRefsKey fileXRefsKey = fileXRefs.getKey();
    List<XRefKey> xRefsKeys =
        fileXRefsKey.getXrefs().stream().map(xRef -> xRef.getKey()).collect(Collectors.toList());

    ic.logger.indentedLog("");
    ic.logger.indentedLog("FileXRefs");
    ic.logger.increaseIndent();
    ic.logger.indentedLog("File: " + fileXRefsKey.getFile().getKey());
    ic.logger.indentedLog("Xrefs");
    ic.logger.increaseIndent();
    xRefsKeys.stream()
        .filter(xRefKey -> xRefKey.getTarget() != null)
        .filter(xRefKey -> xRefKey.getTarget().isSetDefinition_())
        .forEach(
            xRefKey -> {
              ic.logger.indentedLog("XRefKind: definition");
              DebugUtils.debugLogQName(ic, xRefKey.getTarget().getDefinition_());
              DebugUtils.debugLogRanges(ic, xRefKey.getRanges());
            });

    xRefsKeys.stream()
        .filter(xRefKey -> xRefKey.getTarget() != null)
        .filter(xRefKey -> xRefKey.getTarget().isSetCtor_())
        .forEach(
            xRefKey -> {
              ic.logger.indentedLog("XRefKind: ctor");
              DebugUtils.debugLogMethodName(ic, xRefKey.getTarget().getCtor_());
              DebugUtils.debugLogRanges(ic, xRefKey.getRanges());
            });

    xRefsKeys.stream()
        .filter(xRefKey -> xRefKey.getTarget() != null)
        .filter(xRefKey -> xRefKey.getTarget().isSetMethod_())
        .forEach(
            xRefKey -> {
              ic.logger.indentedLog("XRefKind: method");
              DebugUtils.debugLogMethodName(ic, xRefKey.getTarget().getMethod_());
              DebugUtils.debugLogRanges(ic, xRefKey.getRanges());
            });

    xRefsKeys.stream()
        .filter(xRefKey -> xRefKey.getTarget() != null)
        .filter(xRefKey -> xRefKey.getTarget().isSetField_())
        .forEach(
            xRefKey -> {
              ic.logger.indentedLog("XRefKind: field");
              DebugUtils.debugLogQName(ic, xRefKey.getTarget().getField_());
              DebugUtils.debugLogRanges(ic, xRefKey.getRanges());
            });
    ic.logger.decreaseIndent();
    ic.logger.decreaseIndent();
    ic.logger.indentedLog("");
  }

  public static void debugLogMethodName(IndexerContext ic, MethodName mName) {
    ic.logger.indentedLog("MethodName");
    ic.logger.increaseIndent();
    DebugUtils.debugLogQName(ic, mName.getKey().getName());
    ic.logger.decreaseIndent();
    ic.logger.indentedLog("Signature " + mName.getKey().getSignature().toString());
    ic.logger.decreaseIndent();
  }

  public static void debugLogQName(IndexerContext ic, QName qName) {
    ic.logger.indentedLog("QName");
    ic.logger.increaseIndent();
    ic.logger.indentedLog("Name: " + qName.getKey().getName().toString());
    if (qName.getKey().getContext() != null) {
      ic.logger.indentedLog("Context: " + qName.getKey().getContext().toString());
    }
    ic.logger.decreaseIndent();
  }

  public static void debugLogRanges(IndexerContext ic, List<ByteSpan> ranges) {
    ic.logger.indentedLog("Ranges");
    ic.logger.increaseIndent();
    for (ByteSpan byteSpan : ranges) {
      ic.logger.indentedLog(byteSpan.getStart() + " -> " + byteSpan.getLength());
    }
    ic.logger.decreaseIndent();
  }
}
