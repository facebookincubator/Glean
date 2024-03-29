/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;
using Serilog;

namespace Glean.Indexer.Schema.CSharp;

public record Definition
    ( Type? Type = default
    , MethodFact? Method = default
    , ParameterFact? Parameter = default
    , FieldFact? Field = default
    , PropertyFact? Property = default
    , LocalFact? Local = default
    )
{
    public static bool TryFromSymbol(ISymbol symbol, out Definition? result)
    {
        switch (symbol) {
            case ITypeSymbol typeSymbol:
                if (Type.TryFromSymbol(typeSymbol, out var type) && type != null)
                {
                    result = new Definition(Type: type);
                    return true;
                }
                break;
            case IMethodSymbol methodSymbol:
                if (MethodFact.TryFromSymbol(methodSymbol, out var method) && method != null)
                {
                    result = new Definition(Method: method);
                    return true;
                }
                break;
            case IParameterSymbol parameterSymbol:
                if (ParameterFact.TryFromSymbol(parameterSymbol, out var parameter) && parameter != null)
                {
                    result = new Definition(Parameter: parameter);
                    return true;
                }
                break;
            case IFieldSymbol fieldSymbol:
                if (FieldFact.TryFromSymbol(fieldSymbol, out var field) && field != null)
                {
                    result = new Definition(Field: field);
                    return true;
                }
                break;
            case IPropertySymbol propertySymbol:
                if (PropertyFact.TryFromSymbol(propertySymbol, out var property) && property != null)
                {
                    result = new Definition(Property: property);
                    return true;
                }
                break;
            case ILocalSymbol localSymbol:
                if (LocalFact.TryFromSymbol(localSymbol, out var local) && local != null)
                {
                    result = new Definition(Local: local);
                    return true;
                }
                break;
            default:
                Log.Debug($"Unsupported symbol {symbol}");
                break;
        }

        Log.Debug($"Failed to create definition for {symbol}");
        result = default;
        return false;
    }
}
