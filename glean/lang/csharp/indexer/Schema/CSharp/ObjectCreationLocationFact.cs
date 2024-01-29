// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

namespace Indexer.Schema.CSharp;

public record struct ObjectCreationLocationFactKey
    ( Type Type
    , MethodFact Constructor
    , Location Location
    );

public record ObjectCreationLocationFact(ObjectCreationLocationFactKey Key)
    : FactWithKey<ObjectCreationLocationFactKey>(Predicate.ObjectCreationLocation, Key);
