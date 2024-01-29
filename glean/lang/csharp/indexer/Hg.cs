/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace Glean.Indexer;

using System.Diagnostics;
using System.IO;

public static class Hg
{
    public static string RepoRoot { get; }

    public static string GetRepoRootRelativePath(string path)
    {
        return Path.GetRelativePath(RepoRoot, path);
    }

    static Hg()
    {
        var command = "hg root";

        var process = new Process {
            StartInfo = new ProcessStartInfo {
                FileName = "/bin/bash",
                Arguments = $"-c \"{command}\"",
                RedirectStandardOutput = true,
                UseShellExecute = false,
                CreateNoWindow = true,
            }
        };

        process.Start();
        string output = process.StandardOutput.ReadToEnd();

        process.WaitForExit();
        RepoRoot = output.Trim();
    }
}
