// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.predicates;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public abstract class Predicate<T> {

  private final String name;

  public Predicate(String name) {
    this.name = name;
  }

  public abstract Predicate<T> addFact(T fact);

  public String getName() {
    return this.name;
  }

  public abstract Collection<T> getFacts();

  public String serialize() {
    List<String> serializedFacts =
        getFacts().stream()
            .map(
                fact -> {
                  // This may raise exceptions, however catching and returning a blank value causes
                  // an error in glean parsing
                  // try {
                  return SwiftSerializer.serializeJSON(fact);
                  // } catch (Exception exp) {
                  // TODO T63302428 -- Better handle errors during serialization
                  //  return null;
                  // }
                })
            .filter(jsonByteArray -> jsonByteArray != null)
            .map(jsonByteArray -> new String(jsonByteArray))
            .collect(Collectors.toList());

    String factsStringValue = "[" + String.join(", ", serializedFacts) + "]";
    StringBuilder sb = new StringBuilder();
    sb.append("{");
    sb.append("\"predicate\": \"").append(name).append("\"");
    sb.append(", \"facts\": ").append(factsStringValue);
    sb.append("}");

    return sb.toString();
  }
}
