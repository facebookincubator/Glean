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

public record struct PropertyFactKey
    ( NameFact Name
    , NamedType? ContainingType
    , Type Type
    , ParameterFact[] Parameters
    , MethodFact? GetMethod
    , MethodFact? SetMethod
    , bool isStatic
    , bool isIndexer
    );

public record PropertyFact(PropertyFactKey Key) : FactWithKey<PropertyFactKey>(Predicate.Property, Key)
{
    public static bool TryFromSymbol(IPropertySymbol symbol, out PropertyFact? result)
    {
        var name = new NameFact(symbol.Name);

        if (!Type.TryFromSymbol(symbol.Type, out var type) || type == null)
        {
            result = default;
            return false;
        }

        if (!NamedType.TryFromSymbol(symbol.ContainingType, out var containingType))
        {
            // TODO: all documentation say that a property must be defined within the context of a containing type.
            // Why we get a null here is unknown. But adding a null value for containingType will cause glean to fail.
            if (symbol.ContainingType == null)
            {
                Log.Information($"Property {symbol.Name} - ContainingType is null.");
            }
            else
            {
                Log.Information($"Property {symbol.Name} - Unable to determine ContainingType: {symbol.ContainingType.ToDisplayString()}");
            }
            result = default;
            return false;
        }

        MethodFact? getMethod = null;
        if (symbol.GetMethod != null)
        {
            MethodFact.TryFromSymbol(symbol.GetMethod, out getMethod);
        }

        MethodFact? setMethod = null;
        if (symbol.SetMethod != null)
        {
            MethodFact.TryFromSymbol(symbol.SetMethod, out setMethod);
        }

        var parameters = new List<ParameterFact>(){};
        foreach (var parameterSymbol in symbol.Parameters)
        {
            if (ParameterFact.TryFromSymbol(parameterSymbol, out var parameter) && parameter != null)
            {
                parameters.Add(parameter);
            }
        }

        var key = new PropertyFactKey
            ( name
            , containingType
            , type
            , parameters.ToArray()
            , getMethod
            , setMethod
            , symbol.IsStatic
            , symbol.IsIndexer
            );

        result = new PropertyFact(key);
        return true;
    }
}
