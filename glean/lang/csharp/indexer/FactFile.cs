/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Indexer;
using Glean.Indexer.Schema;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Serilog;

namespace Glean.Indexer;

public static class FactFile
{
    private static HashSet<string> uniqueFileNames = new HashSet<string>();

    /*
     * The set of disallowed characters on Unix is a subset of those disallowed on Windows, so we use the
     * stricter set (instead of using the platform-dependent Path.GetInvalidFileNameChars)
     */
    public static char[] GetInvalidFileNameChars() => new char[]
    {
        '\"', '<', '>', '|', '\0', ':', '*', '?', '\\', '/',
        (char)1,  (char)2,  (char)3,  (char)4,  (char)5,  (char)6,  (char)7,  (char)8,  (char)9,  (char)10,
        (char)11, (char)12, (char)13, (char)14, (char)15, (char)16, (char)17, (char)18, (char)19, (char)20,
        (char)21, (char)22, (char)23, (char)24, (char)25, (char)26, (char)27, (char)28, (char)29, (char)30,
        (char)31,
    };

    public static string CleanFileName(string fileName)
    {
        string invalidChars = Regex.Escape(new string(GetInvalidFileNameChars()));
        string invalidRegexString = $"[{invalidChars}]+";

        return Regex.Replace(fileName, invalidRegexString, "_");
    }

    public static string DisambiguateFileName(string[] pathParts, int startIndex)
    {
        string filename = CleanFileName(string.Join("_", pathParts, startIndex, pathParts.Length - startIndex));

        if (!uniqueFileNames.Contains(filename))
        {
            uniqueFileNames.Add(filename);
            return filename;
        }
        else
        {
            return DisambiguateFileName(pathParts, startIndex - 1);
        }
    }

    public static string GetFactFileNameForProject(string projectPath, int? shard)
    {
        string[] pathParts = projectPath.Split(Path.DirectorySeparatorChar);
        var baseFileName = DisambiguateFileName(pathParts, pathParts.Length - 1);

        return shard == null ? $"{baseFileName}.json" : $"{baseFileName}_{shard}.json";
    }

    public static void Write(string outputPath, string projectPath, IEnumerable<Fact> facts, int? shard = null)
    {
        var output = facts
            .GroupBy(fact => fact.Predicate)
            .ToDictionary(
                group => group.Key,
                group => group
                .Select(fact => GleanSerializer.Serialize(fact))
                .ToList()
            )
            .OrderBy(entry => entry.Key)
            .Select(entry => new JObject()
            {
                { "predicate", entry.Key.GetFullName() },
                { "facts", new JArray(entry.Value) }
            });

        var factFileName = FactFile.GetFactFileNameForProject(projectPath, shard);
        var factFilePath = Path.Join(outputPath, factFileName);

        Log.Information($"Wrote {facts.Count()} facts to {factFilePath}");
        File.WriteAllText
        (
            factFilePath,
            GleanSerializer.Encode(output)
        );
    }
}
