/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System.Linq;
using Microsoft.CodeAnalysis;

namespace Glean.Indexer.Schema.CSharp;

public record struct ParameterFactKey
    ( NameFact Name
    , Type Type
    , RefKind RefKind
    , bool IsThis
    , bool IsParams
    , bool IsOptional
    );

public record ParameterFact(ParameterFactKey Key) : FactWithKey<ParameterFactKey>(Predicate.Method, Key)
{
    public static bool TryFromSymbol(IParameterSymbol symbol, out ParameterFact? result)
    {
        var name = new NameFact(symbol.Name);
        if (Type.TryFromSymbol(symbol.Type, out var type) && type != null)
        {
            var key = new ParameterFactKey
                ( name
                , type
                , symbol.RefKind
                , symbol.IsThis
                , symbol.IsParams
                , symbol.IsOptional
                );

            result = new ParameterFact(key);
            return true;
        }

        result = default;
        return false;
    }
}
