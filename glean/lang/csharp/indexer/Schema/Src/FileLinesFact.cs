// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Serilog;
using System.Linq;

namespace Indexer.Schema.Src;

public record struct FileLinesFactKey
    ( FileFact File
    , ulong[] Lengths
    , bool EndsInNewline
    , bool HasUnicodeOrTabs
    );

public record FileLinesFact(FileLinesFactKey Key) : FactWithKey<FileLinesFactKey>(Predicate.FileLines, Key)
{
    public static bool TryFromLocation(Location location, out FileLinesFact? result)
    {
        var sourceTree = location.SourceTree;
        if (sourceTree == null)
        {
            Log.Error("Invalid location: not in a syntax tree");
            result = default;
            return false;
        }

        var absolutePath = sourceTree.FilePath;
        var repoRootRelativePath = Hg.GetRepoRootRelativePath(absolutePath);
        var lines = sourceTree.GetText().Lines;
        var lengths = lines.Select(l => (ulong)l.SpanIncludingLineBreak.Length).ToArray();
        var lastLine = lines.LastOrDefault();
        var endsInNewline = lastLine != null
            ? lastLine.SpanIncludingLineBreak.Length == 0 || lastLine.EndIncludingLineBreak != lastLine.End
            : false;
        var key = new FileLinesFactKey
            ( new FileFact(repoRootRelativePath)
            , lengths
            , endsInNewline
            , HasUnicodeOrTabs: true
            );

        result = new FileLinesFact(key);
        return true;
    }
}
