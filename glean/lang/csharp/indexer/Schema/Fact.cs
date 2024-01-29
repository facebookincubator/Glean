// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

namespace Indexer.Schema;

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
