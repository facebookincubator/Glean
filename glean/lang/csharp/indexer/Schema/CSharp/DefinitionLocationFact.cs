/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using System.Linq;

namespace Glean.Indexer.Schema.CSharp;

public record struct DefinitionLocationFactKey
    ( Definition Definition
    , Location Location
    );

public record DefinitionLocationFact(DefinitionLocationFactKey Key)
    : FactWithKey<DefinitionLocationFactKey>(Predicate.DefinitionLocation, Key)
{
    public static bool TryFromSymbol(ISymbol symbol, out DefinitionLocationFact[]? result)
    {
        if (Definition.TryFromSymbol(symbol, out var definition) && definition is not null)
        {
            var facts = new List<DefinitionLocationFact>();
            foreach (var symbolLocation in symbol.Locations)
            {
                if (Location.TryFromLocation(symbolLocation, out var location) && location is not null)
                {
                    var key = new DefinitionLocationFactKey(definition, location);
                    facts.Add(new DefinitionLocationFact(key));
                    continue;
                }

                result = default;
                return false;
            }

            result = facts.ToArray();
            return true;
        }

        result = default;
        return false;
    }
}
