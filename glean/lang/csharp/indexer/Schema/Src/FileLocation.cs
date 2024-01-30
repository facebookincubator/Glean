/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;
using Serilog;

namespace Glean.Indexer.Schema.Src;

public record FileLocation(FileFact File, ByteSpan Span)
{
    public static bool TryFromLocation(Location location, out FileLocation? fileLocation)
    {
        var absolutePath = location.SourceTree?.FilePath;
        if (absolutePath == null)
        {
            Log.Debug("Invalid location: not in a syntax tree");
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
