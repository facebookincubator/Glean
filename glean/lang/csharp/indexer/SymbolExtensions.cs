/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System.Text;
using Microsoft.CodeAnalysis;

namespace Glean.Indexer;

public static class SymbolExtensions
{
    public static bool IsGlobalNamespaceSymbol(this ISymbol symbol)
    {
        return symbol is INamespaceSymbol namespaceSymbol && namespaceSymbol.IsGlobalNamespace;
    }
}
