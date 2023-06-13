// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.predicates;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class SetPredicate<T> extends Predicate<T> {

  protected Map<Integer, T> facts = new ConcurrentHashMap<>();

  public SetPredicate(String name) {
    super(name);
  }

  @Override
  public Predicate<T> addFact(T fact) {
    facts.put(fact.hashCode(), fact);
    return this;
  }

  @Override
  public Collection<T> getFacts() {
    return facts.values();
  }
}
