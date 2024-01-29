// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

namespace Indexer.Schema.CSharp;

public record struct MethodInvocationLocationFactKey
    ( MethodFact Method
    , Location Location
    );

public record MethodInvocationLocationFact(MethodInvocationLocationFactKey Key)
    : FactWithKey<MethodInvocationLocationFactKey>(Predicate.MethodInvocationLocation, Key);
