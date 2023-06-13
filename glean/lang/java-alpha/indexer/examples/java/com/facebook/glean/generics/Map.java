// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.generics;

public class Map<K, V> {
  private final int size = 2048;
  private final Object[] buckets = new Object[size];
  private MapKind myKind;

  public void put(K key, V value) {
    int bucket = key.hashCode() % size;
    if (myKind == MapKind.StringMap) {
      bucket++;
    } else if (myKind == MapKind.EmptyMap) {
      bucket = 0;
    }
    buckets[bucket] = new Entry(key, value);
  }

  public V get(K key) {
    int bucket = key.hashCode() % size;
    return ((Entry) buckets[bucket]).v;
  }

  public boolean has(K key) {
    return get(key) != null;
  }

  public class Entry {
    public final K k;
    public final V v;

    public Entry(K k, V v) {
      this.k = k;
      this.v = v;
    }
  }

  public interface Foo<K, V> {
    Map<K, V> getMap();
  }

  enum MapKind {
    StringMap,
    EmptyMap,
    IntMap
  }
}
