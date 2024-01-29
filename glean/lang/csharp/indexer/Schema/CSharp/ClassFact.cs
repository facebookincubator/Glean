// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Serilog;
using System.Linq;

namespace Indexer.Schema.CSharp;

public record struct ClassFactKey
    ( FullNameFact Name
    , ClassFact? BaseType
    , TypeParameterFact[] TypeParameters
    , NamedType? ContainingType
    , Accessibility DeclaredAccessibility
    , bool IsAbstract
    , bool IsStatic
    );

public record ClassFact(ClassFactKey Key) : FactWithKey<ClassFactKey>(Predicate.Class, Key)
{
    public static bool TryFromSymbol(INamedTypeSymbol symbol, out ClassFact? result)
    {
        if (FullNameFact.TryFromSymbol(symbol, out var name) && name != null)
        {
            ClassFact? baseType = default;
            if (symbol.BaseType != null)
            {
                if (!ClassFact.TryFromSymbol(symbol.BaseType, out baseType))
                {
                    result = default;
                    return false;
                }
            }

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

            var key = new ClassFactKey
                ( name
                , baseType
                , typeParameters
                , containingType
                , symbol.DeclaredAccessibility
                , symbol.IsAbstract
                , symbol.IsStatic
                );

            result = new ClassFact(key);
            return true;
        }

        result = default;
        return false;
    }
}
