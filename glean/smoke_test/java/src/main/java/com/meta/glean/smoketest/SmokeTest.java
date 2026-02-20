/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.meta.glean.smoketest;

import java.io.Closeable;
import java.io.IOException;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Sample Java class used by the Glean Java indexer smoke test.
 *
 * <p>This file exists to validate that the Glean Java indexer can successfully index fbcode Java
 * targets. It exercises a broad range of Java language features to ensure the indexer produces
 * correct cross-reference facts, including:
 *
 * <ul>
 *   <li>Generics (bounded, wildcards, recursive type bounds)
 *   <li>Interfaces (with default and static methods, functional interfaces)
 *   <li>Enums (with fields, constructors, abstract methods, constant-specific bodies)
 *   <li>Inner classes, static nested classes, anonymous classes
 *   <li>Custom annotations with retention policies
 *   <li>Lambdas and method references
 *   <li>Abstract classes and inheritance hierarchies
 *   <li>Exception handling (custom checked/unchecked exceptions, try-with-resources)
 *   <li>Iterator/Iterable patterns
 *   <li>Builder pattern
 *   <li>Varargs
 *   <li>Static and instance initializers
 *   <li>Streams and Optional
 * </ul>
 *
 * <p>Do not modify without updating the smoke test at
 * tools/skycastle/workflows2/glean/fbsource_smoke_test_java.sky
 */
public class SmokeTest {

  // ---------------------------------------------------------------------------
  // Custom annotations
  // ---------------------------------------------------------------------------

  @Retention(RetentionPolicy.RUNTIME)
  @Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD})
  public @interface Tracked {
    String owner() default "unassigned";

    int priority() default 0;
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  public @interface VisibleForTesting {}

  // ---------------------------------------------------------------------------
  // Enum with fields, constructor, abstract method, and constant-specific bodies
  // ---------------------------------------------------------------------------

  @Tracked(owner = "infra", priority = 1)
  public enum Status {
    CREATED("created", false) {
      @Override
      public Status next() {
        return INITIALIZED;
      }
    },
    INITIALIZED("initialized", false) {
      @Override
      public Status next() {
        return RUNNING;
      }
    },
    RUNNING("running", false) {
      @Override
      public Status next() {
        return STOPPED;
      }
    },
    STOPPED("stopped", true) {
      @Override
      public Status next() {
        return this;
      }
    };

    private final String label;
    private final boolean terminal;

    Status(String label, boolean terminal) {
      this.label = label;
      this.terminal = terminal;
    }

    public String getLabel() {
      return label;
    }

    public boolean isTerminal() {
      return terminal;
    }

    public abstract Status next();
  }

  // ---------------------------------------------------------------------------
  // Functional interface + interface with default and static methods
  // ---------------------------------------------------------------------------

  @FunctionalInterface
  public interface Transformer<I, O> {
    O transform(I input);
  }

  public interface StatusListener {
    void onStatusChanged(Status oldStatus, Status newStatus);

    default void onError(Status status, Throwable error) {
      // default no-op
    }

    static StatusListener noOp() {
      return (oldStatus, newStatus) -> {};
    }
  }

  // ---------------------------------------------------------------------------
  // Interface with recursive generic bound (self-referential)
  // ---------------------------------------------------------------------------

  public interface Identifiable<T extends Identifiable<T>> {
    String getId();

    int compareTo(T other);
  }

  // ---------------------------------------------------------------------------
  // Custom exceptions
  // ---------------------------------------------------------------------------

  public static class RegistryException extends Exception {
    private final String key;

    public RegistryException(String key, String message) {
      super(message);
      this.key = key;
    }

    public RegistryException(String key, String message, Throwable cause) {
      super(message, cause);
      this.key = key;
    }

    public String getKey() {
      return key;
    }
  }

  public static class DuplicateEntryException extends RegistryException {
    public DuplicateEntryException(String key) {
      super(key, "Duplicate entry for key: " + key);
    }
  }

  // ---------------------------------------------------------------------------
  // Abstract base class with generics
  // ---------------------------------------------------------------------------

  public abstract static class AbstractEntry<T> implements Identifiable<AbstractEntry<T>> {
    protected final String key;
    protected final T value;
    protected final long timestamp;

    protected AbstractEntry(String key, T value) {
      this.key = Objects.requireNonNull(key, "key must not be null");
      this.value = value;
      this.timestamp = System.currentTimeMillis();
    }

    public String getKey() {
      return key;
    }

    public T getValue() {
      return value;
    }

    public long getTimestamp() {
      return timestamp;
    }

    @Override
    public String getId() {
      return key;
    }

    @Override
    public int compareTo(AbstractEntry<T> other) {
      return this.key.compareTo(other.key);
    }

    public abstract String describe();
  }

  // ---------------------------------------------------------------------------
  // Concrete generic entry with builder pattern
  // ---------------------------------------------------------------------------

  @Tracked(owner = "storage", priority = 2)
  public static class Entry<T> extends AbstractEntry<T> {
    private final Map<String, String> metadata;

    private Entry(Builder<T> builder) {
      super(builder.key, builder.value);
      this.metadata = Collections.unmodifiableMap(new HashMap<>(builder.metadata));
    }

    public Map<String, String> getMetadata() {
      return metadata;
    }

    public Optional<String> getMetadataValue(String metaKey) {
      return Optional.ofNullable(metadata.get(metaKey));
    }

    public <R> R mapValue(Function<? super T, ? extends R> mapper) {
      return mapper.apply(value);
    }

    @Override
    public String describe() {
      return "Entry{key=" + key + ", value=" + value + ", meta=" + metadata.size() + " tags}";
    }

    @Override
    public String toString() {
      return describe();
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof Entry)) return false;
      Entry<?> entry = (Entry<?>) o;
      return key.equals(entry.key) && Objects.equals(value, entry.value);
    }

    @Override
    public int hashCode() {
      return Objects.hash(key, value);
    }

    public static <T> Builder<T> builder(String key, T value) {
      return new Builder<>(key, value);
    }

    public static class Builder<T> {
      private final String key;
      private final T value;
      private final Map<String, String> metadata = new HashMap<>();

      private Builder(String key, T value) {
        this.key = Objects.requireNonNull(key);
        this.value = value;
      }

      public Builder<T> withMetadata(String metaKey, String metaValue) {
        metadata.put(metaKey, metaValue);
        return this;
      }

      public Entry<T> build() {
        return new Entry<>(this);
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Generic registry with iterator, try-with-resources, varargs, streams
  // ---------------------------------------------------------------------------

  public static class Registry<T> implements Iterable<Entry<T>>, Closeable {
    private final List<Entry<T>> entries = new ArrayList<>();
    private final List<StatusListener> listeners = new ArrayList<>();
    private Status status;
    private boolean closed = false;

    {
      // instance initializer
      status = Status.CREATED;
    }

    public Registry() {}

    public void addListener(StatusListener listener) {
      listeners.add(Objects.requireNonNull(listener));
    }

    private void transitionTo(Status newStatus) {
      Status old = this.status;
      this.status = newStatus;
      for (StatusListener listener : listeners) {
        try {
          listener.onStatusChanged(old, newStatus);
        } catch (RuntimeException e) {
          listener.onError(newStatus, e);
        }
      }
    }

    public void initialize() {
      transitionTo(Status.INITIALIZED);
    }

    public void start() {
      transitionTo(Status.RUNNING);
    }

    public void register(Entry<T> entry) throws DuplicateEntryException {
      ensureOpen();
      for (Entry<T> existing : entries) {
        if (existing.getKey().equals(entry.getKey())) {
          throw new DuplicateEntryException(entry.getKey());
        }
      }
      entries.add(entry);
    }

    @SafeVarargs
    public final void registerAll(Entry<T>... entriesToAdd) throws DuplicateEntryException {
      for (Entry<T> entry : entriesToAdd) {
        register(entry);
      }
    }

    public Optional<Entry<T>> find(String key) {
      return entries.stream().filter(e -> e.getKey().equals(key)).findFirst();
    }

    public <R> List<R> mapEntries(Function<? super Entry<T>, ? extends R> mapper) {
      return entries.stream().map(mapper).collect(Collectors.toList());
    }

    public List<Entry<T>> filter(Predicate<? super Entry<T>> predicate) {
      return entries.stream().filter(predicate).collect(Collectors.toList());
    }

    public void forEach(Consumer<? super Entry<T>> action) {
      entries.forEach(action);
    }

    public List<Entry<T>> sorted(Comparator<? super Entry<T>> comparator) {
      return entries.stream().sorted(comparator).collect(Collectors.toList());
    }

    public List<Entry<T>> sortedByKey() {
      return sorted(Comparator.comparing(AbstractEntry::getKey));
    }

    public int size() {
      return entries.size();
    }

    public boolean isEmpty() {
      return entries.isEmpty();
    }

    public Status getStatus() {
      return status;
    }

    @Override
    public Iterator<Entry<T>> iterator() {
      return new RegistryIterator();
    }

    @Override
    public void close() throws IOException {
      if (!closed) {
        closed = true;
        transitionTo(Status.STOPPED);
        entries.clear();
      }
    }

    private void ensureOpen() {
      if (closed) {
        throw new IllegalStateException("Registry is closed");
      }
    }

    private class RegistryIterator implements Iterator<Entry<T>> {
      private int cursor = 0;

      @Override
      public boolean hasNext() {
        return cursor < entries.size();
      }

      @Override
      public Entry<T> next() {
        if (!hasNext()) {
          throw new NoSuchElementException();
        }
        return entries.get(cursor++);
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException("remove not supported");
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Bounded wildcard utility methods
  // ---------------------------------------------------------------------------

  public static <T extends Comparable<? super T>> Entry<T> max(List<? extends Entry<T>> entries) {
    if (entries.isEmpty()) {
      throw new IllegalArgumentException("entries must not be empty");
    }
    Entry<T> best = entries.get(0);
    for (Entry<T> entry : entries) {
      if (entry.getValue().compareTo(best.getValue()) > 0) {
        best = entry;
      }
    }
    return best;
  }

  public static void addAll(
      List<? super Entry<String>> target, List<? extends Entry<String>> source) {
    target.addAll(source);
  }

  // ---------------------------------------------------------------------------
  // Anonymous class and lambda usage
  // ---------------------------------------------------------------------------

  @VisibleForTesting
  static StatusListener createLoggingListener() {
    return new StatusListener() {
      @Override
      public void onStatusChanged(Status oldStatus, Status newStatus) {
        System.out.println(oldStatus.getLabel() + " -> " + newStatus.getLabel());
      }

      @Override
      public void onError(Status status, Throwable error) {
        System.err.println("Error in state " + status.getLabel() + ": " + error.getMessage());
      }
    };
  }

  @VisibleForTesting
  static <T> Transformer<Entry<T>, String> keyExtractor() {
    return Entry::getKey;
  }

  // ---------------------------------------------------------------------------
  // Static nested class demonstrating try-with-resources usage
  // ---------------------------------------------------------------------------

  public static class RegistryOperations {

    public static <T> List<String> extractKeys(Entry<T>... entries) {
      List<String> keys = new ArrayList<>();
      for (Entry<T> entry : entries) {
        keys.add(entry.getKey());
      }
      return keys;
    }

    public static <T> void populateAndClose(Registry<T> registry, List<Entry<T>> entries)
        throws RegistryException {
      try (Registry<T> reg = registry) {
        reg.initialize();
        reg.start();
        for (Entry<T> entry : entries) {
          reg.register(entry);
        }
      } catch (DuplicateEntryException e) {
        throw new RegistryException(e.getKey(), "Failed to populate registry", e);
      } catch (IOException e) {
        throw new RegistryException("", "I/O error while closing registry", e);
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Pair class for multi-type generics
  // ---------------------------------------------------------------------------

  public static class Pair<A, B> {
    private final A first;
    private final B second;

    public Pair(A first, B second) {
      this.first = first;
      this.second = second;
    }

    public A getFirst() {
      return first;
    }

    public B getSecond() {
      return second;
    }

    public <C> Pair<A, C> mapSecond(Function<? super B, ? extends C> mapper) {
      return new Pair<>(first, mapper.apply(second));
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof Pair)) return false;
      Pair<?, ?> pair = (Pair<?, ?>) o;
      return Objects.equals(first, pair.first) && Objects.equals(second, pair.second);
    }

    @Override
    public int hashCode() {
      return Objects.hash(first, second);
    }

    @Override
    public String toString() {
      return "(" + first + ", " + second + ")";
    }
  }

  // ---------------------------------------------------------------------------
  // Static initializer block
  // ---------------------------------------------------------------------------

  private static final Map<String, String> DEFAULTS;

  static {
    Map<String, String> map = new HashMap<>();
    map.put("version", "1.0");
    map.put("author", "smoketest");
    DEFAULTS = Collections.unmodifiableMap(map);
  }

  public static Map<String, String> getDefaults() {
    return DEFAULTS;
  }

  // ---------------------------------------------------------------------------
  // Intersection type bound
  // ---------------------------------------------------------------------------

  public static <T extends Comparable<T> & Closeable> T minCloseable(List<T> items) {
    if (items.isEmpty()) {
      throw new IllegalArgumentException("items must not be empty");
    }
    T min = items.get(0);
    for (T item : items) {
      if (item.compareTo(min) < 0) {
        min = item;
      }
    }
    return min;
  }

  // ---------------------------------------------------------------------------
  // Method that uses multiple features together
  // ---------------------------------------------------------------------------

  @Tracked(owner = "test")
  @VisibleForTesting
  static Pair<Integer, List<String>> summarize(Registry<String> registry) {
    List<String> keys =
        registry.mapEntries(Entry::getKey).stream().sorted().collect(Collectors.toList());
    return new Pair<>(registry.size(), keys);
  }

  // ---------------------------------------------------------------------------
  // Main method — exercises the features above for manual verification
  // ---------------------------------------------------------------------------

  public static void main(String[] args) throws Exception {
    System.out.println("=== SmokeTest: verifying Java features ===");

    // Enum with constant-specific bodies
    for (Status s : Status.values()) {
      System.out.println(
          "  Status: "
              + s.getLabel()
              + " terminal="
              + s.isTerminal()
              + " next="
              + s.next().getLabel());
    }

    // Builder pattern, generics, metadata
    Entry<String> alpha =
        Entry.<String>builder("alpha", "first").withMetadata("source", "test").build();
    Entry<String> beta =
        Entry.<String>builder("beta", "second").withMetadata("source", "test").build();
    Entry<String> gamma = Entry.<String>builder("gamma", "third").build();

    System.out.println("  Built entries: " + alpha + ", " + beta + ", " + gamma);

    // Registry with listeners, try-with-resources, iterator
    Registry<String> registry = new Registry<>();
    registry.addListener(createLoggingListener());
    registry.addListener(StatusListener.noOp());
    registry.initialize();
    registry.start();
    registry.register(alpha);
    registry.register(beta);
    registry.register(gamma);

    // Duplicate detection
    try {
      registry.register(Entry.<String>builder("alpha", "duplicate").build());
      System.err.println("  ERROR: should have thrown DuplicateEntryException");
    } catch (DuplicateEntryException e) {
      System.out.println("  Caught expected: " + e.getMessage());
    }

    // Stream operations, lambdas, method references
    Transformer<Entry<String>, String> extractor = keyExtractor();
    System.out.println("  Extractor on alpha: " + extractor.transform(alpha));

    List<Entry<String>> filtered = registry.filter(e -> e.getKey().compareTo("b") >= 0);
    System.out.println("  Filtered (key >= 'b'): " + filtered);

    List<String> keys = registry.mapEntries(Entry::getKey);
    System.out.println("  All keys: " + keys);

    List<Entry<String>> sorted = registry.sortedByKey();
    System.out.println("  Sorted: " + sorted);

    // Bounded wildcard utility
    Entry<String> maxEntry = max(sorted);
    System.out.println("  Max by value: " + maxEntry);

    // Pair, mapSecond
    Pair<Integer, List<String>> summary = summarize(registry);
    Pair<Integer, String> joined = summary.mapSecond(l -> String.join(",", l));
    System.out.println("  Summary: count=" + joined.getFirst() + " keys=" + joined.getSecond());

    // Optional, metadata lookup
    Optional<String> meta = alpha.getMetadataValue("source");
    System.out.println("  Alpha metadata 'source': " + meta.orElse("missing"));
    System.out.println(
        "  Alpha metadata 'nope': " + alpha.getMetadataValue("nope").orElse("missing"));

    // mapValue with lambda
    String upper = alpha.mapValue(String::toUpperCase);
    System.out.println("  Alpha value uppercased: " + upper);

    // Iterator pattern
    System.out.print("  Iterator walk:");
    for (Entry<String> e : registry) {
      System.out.print(" " + e.getKey());
    }
    System.out.println();

    // Static defaults
    System.out.println("  Defaults: " + getDefaults());

    // Close (try-with-resources)
    registry.close();
    System.out.println("  Registry closed, status=" + registry.getStatus().getLabel());

    // Verify closed state
    try {
      registry.register(Entry.<String>builder("fail", "should-fail").build());
      System.err.println("  ERROR: should have thrown IllegalStateException");
    } catch (IllegalStateException e) {
      System.out.println("  Caught expected: " + e.getMessage());
    }

    System.out.println("=== SmokeTest: all checks passed ===");
  }
}
