/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;

namespace Glean.Indexer.Schema.CSharp;

public record struct TypeParameterFactKey
    ( NameFact Name
    , VarianceKind? Variance
    , bool HasNotNullConstraint
    , bool HasReferenceTypeConstraint
    , bool HasValueTypeConstraint
    );

public record TypeParameterFact(TypeParameterFactKey Key) : FactWithKey<TypeParameterFactKey>(Predicate.TypeParameter, Key)
{
    public static TypeParameterFact FromSymbol(ITypeParameterSymbol symbol)
    {
        var key = new TypeParameterFactKey
            ( new NameFact(symbol.Name)
            , symbol.Variance
            , symbol.HasNotNullConstraint
            , symbol.HasReferenceTypeConstraint
            , symbol.HasValueTypeConstraint
            );

        return new TypeParameterFact(key);
    }
}
