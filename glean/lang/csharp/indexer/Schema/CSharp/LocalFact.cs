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
using System.Collections.Generic;

namespace Glean.Indexer.Schema.CSharp;

public record struct LocalFactKey
    ( NameFact Name
    , Type Type
    , MethodFact? ContainingMethod
    , RefKind RefKind
    , bool isConst
    );

public record LocalFact(LocalFactKey Key) : FactWithKey<LocalFactKey>(Predicate.Local, Key)
{
    public static bool TryFromSymbol(ILocalSymbol symbol, out LocalFact? result)
    {
        var name = new NameFact(symbol.Name);

        if (!Type.TryFromSymbol(symbol.Type, out var type) || type == null)
        {
            result = default;
            return false;
        }

        IMethodSymbol? methodSymbol = symbol.ContainingSymbol as IMethodSymbol;
        MethodFact? containingMethod = null;
        if (methodSymbol != null)
        {
            MethodFact.TryFromSymbol(methodSymbol, out containingMethod);
        }

        if (containingMethod == null)
        {
            // This is not allowed in schema
            result = default;
            return false;

        }

        var key = new LocalFactKey
            ( name
            , type
            , containingMethod
            , symbol.RefKind
            , symbol.IsConst
            );

        result = new LocalFact(key);
        return true;
    }
}
