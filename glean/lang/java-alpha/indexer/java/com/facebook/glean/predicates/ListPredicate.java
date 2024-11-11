// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.predicates;

import com.facebook.thrift.payload.ThriftSerializable;
import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;

public class ListPredicate<T extends ThriftSerializable> extends Predicate<T> {

  protected Collection<T> facts = new ConcurrentLinkedQueue<>();

  public ListPredicate(String name) {
    super(name);
  }

  @Override
  public Predicate<T> addFact(T fact) {
    facts.add(fact);
    return this;
  }

  @Override
  public Collection<T> getFacts() {
    return facts;
  }
}
