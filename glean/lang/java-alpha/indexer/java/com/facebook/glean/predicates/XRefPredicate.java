// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.predicates;

import com.facebook.glean.schema.java_alpha.XRef;
import com.facebook.glean.schema.java_alpha.XRefKey;
import com.facebook.glean.schema.java_alpha.XRefTarget;
import com.facebook.glean.schema.javakotlin_alpha.MethodName;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.javakotlin_alpha.QNameKey;
import com.facebook.glean.schema.src.ByteSpan;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.stream.Collectors;

public class XRefPredicate extends ListPredicate<XRef> {

  public XRefPredicate(String name) {
    super(name);
  }

  @Override
  public synchronized Predicate<XRef> addFact(XRef fact) {
    return super.addFact(fact);
  }

  /**
   * The schema defines an xref as a single xref target with multiple ranges for where they occur
   *
   * <p>predicate XRef : { target: XRefTarget, ranges: [src.ByteSpan], }
   *
   * <p>However during indexing, we create a fact for each instance of an xref which results in
   * this:
   *
   * <p>predicate XRef : { target: XRefTarget, range: src.ByteSpan, }
   *
   * <p>This method goes through the list of facts, grouping by xRefTarget, and concatenating the
   * ranges array so we match the schema predicate def.
   */
  public synchronized void consolidate() {
    Map<String, XRefKey.Builder> xRefKeyBuilders = new HashMap<>();

    for (XRef fact : facts) {
      QName qName = null;
      MethodName mName = null;
      boolean mNameRequired = false;

      XRefTarget xRefTarget = fact.getKey().getTarget();
      if (xRefTarget != null) {
        if (xRefTarget.isSetDefinition_()) {
          qName = xRefTarget.getDefinition_();
        } else if (xRefTarget.isSetCtor_()) {
          mNameRequired = true;
          mName = xRefTarget.getCtor_();
          qName = mName != null ? mName.getKey().getName() : null;
        } else if (xRefTarget.isSetMethod_()) {
          mNameRequired = true;
          mName = xRefTarget.getMethod_();
          qName = mName != null ? mName.getKey().getName() : null;
        } else if (xRefTarget.isSetField_()) {
          qName = xRefTarget.getField_();
        }
      }

      if (qName == null) {
        throw new RuntimeException("XRefPredicate: Null qName and mName when consolidating");
      }

      String qNameHash = hashName(qName, mName, mNameRequired);

      if (xRefKeyBuilders.containsKey(qNameHash)) {
        if (fact.getKey().getRanges() != null) {
          XRefKey.Builder builder = xRefKeyBuilders.get(qNameHash);
          builder.getRanges().addAll(fact.getKey().getRanges());
        }
      } else {
        XRefKey.Builder builder =
            new XRefKey.Builder().setTarget(fact.getKey().getTarget()).setRanges(new ArrayList<>());
        if (fact.getKey().getRanges() != null) {
          builder.getRanges().addAll(fact.getKey().getRanges());
        }
        xRefKeyBuilders.put(qNameHash, builder);
      }
    }

    this.facts =
        new ConcurrentLinkedQueue<>(
            xRefKeyBuilders.values().stream()
                .map(
                    builder -> {
                      // At this point there can be duplicate ByteRanges in the consolidated list.
                      // Time to dedup.
                      // We don't care about ordering. IE a ByteSpan later in the file can appear
                      // before a byte span earlier in the file after this operation.
                      // Note that ByteSpan properly overrides hash code to account for contents.

                      List<ByteSpan> dedupedRanges =
                          new ArrayList<>(new HashSet<>(builder.getRanges()));
                      builder.setRanges(dedupedRanges);
                      return builder;
                    })
                .map(builder -> builder.build())
                .map(xRefKey -> new XRef.Builder().setKey(xRefKey).build())
                .collect(Collectors.toList()));
  }

  private String hashName(QName qName, MethodName mName, boolean mNameRequired) {
    QNameKey qNameKey = qName.getKey();
    String qNameString = qNameKey.toString();
    String signatureString =
        mNameRequired ? defaultString(mName.getKey().getSignature().toString(), "") : "";
    return qNameString + signatureString;
  }

  private String defaultString(String string, String defaultStr) {
    return string != null ? string : defaultStr;
  }
}
