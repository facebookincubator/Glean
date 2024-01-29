// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Serilog;

namespace Indexer.Schema.Src;

public record FileLocation(FileFact File, ByteSpan Span)
{
    public static bool TryFromLocation(Location location, out FileLocation? fileLocation)
    {
        var absolutePath = location.SourceTree?.FilePath;
        if (absolutePath == null)
        {
            Log.Error("Invalid location: not in a syntax tree");
            fileLocation = default;
            return false;
        }

        var repoRootRelativePath = Hg.GetRepoRootRelativePath(absolutePath);

        var file = new FileFact(repoRootRelativePath);
        var span = new ByteSpan(
            location.SourceSpan.Start,
            location.SourceSpan.Length
        );

        fileLocation = new FileLocation(
            file,
            span
        );
        return true;
    }
}
