// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Indexer;
using Indexer.Schema;
using Indexer.Schema.CSharp;
using System.Linq;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.IO;
using Serilog;

namespace Indexer;

public class FactFile
{
    private static HashSet<string> uniqueFileNames = new HashSet<string>();
    private List<Fact> Facts = new List<Fact>();
    private string ProjectPath = "";
    private string OutputDirectory = "";
    private int FactsLimit = 0;
    private int FileIndex = 0;

    public int FactsTotalCount {get;set;}

    /*
     * The set of disallowed characters on Unix is a subset of those disallowed on Windows, so we use the
     * stricter set (instead of using the platform-dependent Path.GetInvalidFileNameChars)
     */
    public char[] GetInvalidFileNameChars() => new char[]
    {
        '\"', '<', '>', '|', '\0', ':', '*', '?', '\\', '/',
        (char)1,  (char)2,  (char)3,  (char)4,  (char)5,  (char)6,  (char)7,  (char)8,  (char)9,  (char)10,
        (char)11, (char)12, (char)13, (char)14, (char)15, (char)16, (char)17, (char)18, (char)19, (char)20,
        (char)21, (char)22, (char)23, (char)24, (char)25, (char)26, (char)27, (char)28, (char)29, (char)30,
        (char)31,
    };

    public string CleanFileName(string fileName)
    {
        string invalidChars = Regex.Escape(new string(GetInvalidFileNameChars()));
        string invalidRegexString = $"[{invalidChars}]+";

        return Regex.Replace(fileName, invalidRegexString, "_");
    }

    public string DisambiguateFileName(string[] pathParts)
    {
        string filename = CleanFileName(string.Join("_", pathParts, 0, pathParts.Length - 1));
        filename += $"_{FileIndex}";

        if (!uniqueFileNames.Contains(filename))
        {
            uniqueFileNames.Add(filename);
            return filename;
        }
        else
        {
            FileIndex++;
            return DisambiguateFileName(pathParts);
        }
    }

    public string GetFactFileNameForProject(string projectPath)
    {
        string[] pathParts = projectPath.Split(Path.DirectorySeparatorChar);
        return $"{DisambiguateFileName(pathParts)}.json";
    }

    public FactFile(string projectPath, string outputDirectory, int factsLimit)
    {
        Facts = new List<Fact>();
        OutputDirectory = outputDirectory;
        ProjectPath = projectPath;
        FactsLimit = factsLimit;

        FileIndex = 1;
        FactsTotalCount = 0;
    }

    public void AddFact(Fact fact)
    {
        Facts.Add(fact);

        if (FactsLimit > 0 && Facts.Count > FactsLimit)
        {
            CreateFile();
        }
    }

    public void AddFacts(List<DefinitionLocationFact> facts)
    {
        Facts.AddRange(facts);

        if (FactsLimit > 0 && Facts.Count >= FactsLimit)
        {
            CreateFile();
        }
    }

    public void CreateFile()
    {
        if (Facts.Count > 0)
        {
            var factFileName = GetFactFileNameForProject(ProjectPath);
            var factFilePath = Path.Join(OutputDirectory, factFileName);

            var facts = Facts
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

            File.WriteAllTextAsync
            (
                factFilePath,
                GleanSerializer.Encode(facts)
            );

            FactsTotalCount += Facts.Count;
            Log.Information($"Wrote {Facts.Count} facts to {factFilePath}");

        }

        Facts = new List<Fact>();
        FileIndex++;
    }
}
