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

public record NamedType(ClassFact? Class_ = null, StructFact? Struct_ = null, RecordFact? Record_ = null, InterfaceFact? Interface_ = null)
{
    public static bool TryFromSymbol(INamedTypeSymbol symbol, out NamedType? result)
    {
        switch (symbol.TypeKind)
        {
            case TypeKind.Class:
                if ((symbol as ITypeSymbol).IsRecord)
                {
                    if (RecordFact.TryFromSymbol(symbol, out var record_))
                    {
                        result = new NamedType(Record_: record_);
                        return true;
                    }
                }
                else
                {
                    if (ClassFact.TryFromSymbol(symbol, out var class_))
                    {
                        result = new NamedType(Class_: class_);
                        return true;
                    }
                }
                break;
            case TypeKind.Struct:
                if (StructFact.TryFromSymbol(symbol, out var struct_))
                {
                    result = new NamedType(Struct_: struct_);
                    return true;
                }
                break;
            case TypeKind.Interface:
                if (InterfaceFact.TryFromSymbol(symbol, out var interface_))
                {
                    result = new NamedType(Interface_: interface_);
                    return true;
                }
                break;
            default:
                Log.Error($"Unsupported type kind: {symbol.TypeKind}");
                break;
        }

        result = default;
        return false;
    }
}
