// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Indexer.Schema.Src;

namespace Indexer.Schema.CSharp;

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
