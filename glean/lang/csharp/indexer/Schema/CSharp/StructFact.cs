/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;
using Serilog;
using System.Linq;

namespace Glean.Indexer.Schema.CSharp;

public record struct StructFactKey
    ( FullNameFact Name
    , TypeParameterFact[] TypeParameters
    , NamedType? ContainingType
    , Accessibility DeclaredAccessibility
    );

public record StructFact(StructFactKey Key) : FactWithKey<StructFactKey>(Predicate.Struct, Key)
{
    public static bool TryFromSymbol(INamedTypeSymbol symbol, out StructFact? result)
    {
        if (FullNameFact.TryFromSymbol(symbol, out var name) && name != null)
        {
            TypeParameterFact[] typeParameters = symbol
                .TypeParameters
                .Select(typeParameterSymbol => TypeParameterFact.FromSymbol(typeParameterSymbol))
                .ToArray();

            NamedType? containingType = default;
            if (symbol.ContainingType != null)
            {
                if (!NamedType.TryFromSymbol(symbol.ContainingType, out containingType))
                {
                    result = default;
                    return false;
                }
            }

            var key = new StructFactKey
                ( name
                , typeParameters
                , containingType
                , symbol.DeclaredAccessibility
                );

            result = new StructFact(key);
            return true;
        }

        result = default;
        return false;
    }
}
