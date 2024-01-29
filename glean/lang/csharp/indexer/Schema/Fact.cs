/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace Glean.Indexer.Schema;

public abstract record Fact(Predicate Predicate);

public interface IFactWithKey
{
    Predicate Predicate { get; }
    object UntypedKey { get; }
}

public record FactWithKey<TKey>(Predicate Predicate, TKey Key) : Fact(Predicate), IFactWithKey where TKey : notnull
{
    public object UntypedKey { get => (object)Key; }
}
