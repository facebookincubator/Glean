// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Indexer;
using Indexer.Schema;
using Newtonsoft.Json.Linq;

namespace Indexer.Test;

public enum ArbitraryEnum
{
    A = 3,
    B = 1,
    C = 2,
}

public record ArbitraryFact1(string Field1)
    : FactWithKey<string>(Predicate.Arbitrary, Field1);

public record struct ArbitraryFact2Key
    ( string Field1
    , ArbitraryFact1 Field2
    );

public record ArbitraryFact2(ArbitraryFact2Key Key)
    : FactWithKey<ArbitraryFact2Key>(Predicate.Arbitrary, Key);

public record ArbitraryFact3(ArbitraryFact1[] Field1)
    : FactWithKey<ArbitraryFact1[]>(Predicate.Arbitrary, Field1);

public record ArbitraryFact4(ArbitraryEnum[] Field1)
    : FactWithKey<ArbitraryEnum[]>(Predicate.Arbitrary, Field1);

public record ArbitrarySumType(ArbitraryFact1? Option1 = default, ArbitraryFact2? Option2 = default);
