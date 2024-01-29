// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

namespace Indexer.Schema.CSharp;

public record NameFact(string Name) : FactWithKey<string>(Predicate.Name, Name)
{
    public static NameFact Empty()
    {
        return new NameFact(string.Empty);
    }
}
