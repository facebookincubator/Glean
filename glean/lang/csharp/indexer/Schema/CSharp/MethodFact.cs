/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System.Linq;
using Microsoft.CodeAnalysis;
using System.Collections.Generic;

namespace Glean.Indexer.Schema.CSharp;

public record struct MethodFactKey
    ( NameFact Name
    , NamedType ContainingType
    , TypeParameterFact[] TypeParameters
    , ParameterFact[] Parameters
    , bool IsStatic
    , Type ReturnType
    , Accessibility DeclaredAccessibility
    );

public record MethodFact(MethodFactKey Key) : FactWithKey<MethodFactKey>(Predicate.Method, Key)
{
    public static bool TryFromSymbol(IMethodSymbol symbol, out MethodFact? result)
    {
        var name = new NameFact(symbol.Name);

        if (NamedType.TryFromSymbol(symbol.ContainingType, out var containingType) && containingType != null)
        {
            var typeParameters = symbol
                .TypeParameters
                .Select(TypeParameterFact.FromSymbol)
                .ToArray();

            var parameters = new List<ParameterFact>(){};
            foreach (var parameterSymbol in symbol.Parameters)
            {
                if (ParameterFact.TryFromSymbol(parameterSymbol, out var parameter) && parameter != null)
                {
                    parameters.Add(parameter);
                    continue;
                }

                result = default;
                return false;
            }

            var isStatic = symbol.IsStatic;

            if (Type.TryFromSymbol(symbol.ReturnType, out var returnType) && returnType != null)
            {
                var declaredAccessibiliy = symbol.DeclaredAccessibility;

                var key = new MethodFactKey
                    ( name
                    , containingType
                    , typeParameters
                    , parameters.ToArray()
                    , isStatic
                    , returnType
                    , declaredAccessibiliy
                    );

                result = new MethodFact(key);
                return true;
            }
        }

        result = default;
        return false;
    }
}
