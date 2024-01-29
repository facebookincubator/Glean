// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Serilog;

namespace Indexer.Schema.CSharp;

public record Type(NamedType? NamedType = default, TypeParameterFact? TypeParameter = default, ArrayTypeFact? ArrayType = default)
{
    public static bool TryFromSymbol(ITypeSymbol symbol, out Type? result)
    {
        switch (symbol) {
            case INamedTypeSymbol namedTypeSymbol:
                if (NamedType.TryFromSymbol(namedTypeSymbol, out var namedType) && namedType != null)
                {
                    result = new Type(NamedType: namedType);
                    return true;
                }
                break;
            case ITypeParameterSymbol typeParameterSymbol:
                var typeParameter = TypeParameterFact.FromSymbol(typeParameterSymbol);

                result = new Type(TypeParameter: typeParameter);
                return true;
            case IArrayTypeSymbol arrayTypeSymbol:
                if (ArrayTypeFact.TryFromSymbol(arrayTypeSymbol, out var arrayType) && arrayType != null)
                {
                    result = new Type(ArrayType: arrayType);
                    return true;
                }
                break;
            default:
                Log.Error($"Unsupported type {symbol}");
                break;
        }

        result = default;
        return false;
    }
}
