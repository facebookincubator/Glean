/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;
using Glean.Indexer.Schema.Src;

namespace Glean.Indexer.Schema.CSharp;

public record Location(FileLocation FileLocation, FileLinesFact FileLines)
{
    public static bool TryFromLocation(Microsoft.CodeAnalysis.Location location, out Location? result)
    {
        if (FileLocation.TryFromLocation(location, out var fileLocation) &&
            fileLocation is not null &&
            FileLinesFact.TryFromLocation(location, out var fileLines) &&
            fileLines is not null)
        {
            result = new Location(fileLocation, fileLines);
            return true;
        }

        result = default;
        return false;
    }
}
