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

public record struct FieldFactKey
    ( NameFact Name
    , Type Type
    , NamedType? ContainingType
    , Accessibility DeclaredAccessibility
    , bool IsConst
    , bool isReadonly
    , bool isVirtual
    );

public record FieldFact(FieldFactKey Key) : FactWithKey<FieldFactKey>(Predicate.Field, Key)
{
    public static bool TryFromSymbol(IFieldSymbol symbol, out FieldFact? result)
    {
        var name = new NameFact(symbol.Name);

        if (!Type.TryFromSymbol(symbol.Type, out var type) || type == null)
        {
            result = default;
            return false;
        }

        if (!NamedType.TryFromSymbol(symbol.ContainingType, out var containingType))
        {
            // TODO: all documentation say that a field can only exist in a class or a struct.
            // Why we get a null here is unknown. But adding a null value for containingType will cause glean to fail.
            if (symbol.ContainingType == null)
            {
                Log.Information($"{symbol.Name} - ContainingType is null.");
            }
            else
            {
                Log.Information($"{symbol.Name} - Unable to determine ContainingType: {symbol.ContainingType.ToDisplayString()}");
            }
            result = default;
            return false;
        }

        var key = new FieldFactKey
            ( name
            , type
            , containingType
            , symbol.DeclaredAccessibility
            , symbol.IsConst
            , symbol.IsReadOnly
            , symbol.IsVirtual
            );

        result = new FieldFact(key);
        return true;
    }
}
