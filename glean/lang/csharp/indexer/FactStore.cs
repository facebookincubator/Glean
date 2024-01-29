/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System.Collections.Generic;
using Glean.Indexer.Schema;
using System.Linq;
using Newtonsoft.Json.Linq;
using System.IO;
using Serilog;

namespace Glean.Indexer;

public class FactStore
{
    private readonly List<Fact> _facts = new();

    public int CurrentFactCount
    {
        get
        {
            return _facts.Count;
        }
    }

    public int CurrentShardIndex { get; private set; } = 0;
    public int Capacity { get; init; }
    public string OutputPath { get; init; }

    public FactStore(int capacity, string outputPath)
        => (Capacity, OutputPath) = (capacity, outputPath);

    public void Add(Fact fact)
    {
        AddRange(new[] { fact });
    }

    public void AddRange(IEnumerable<Fact> facts)
    {
        var remainingFacts = facts.ToList();
        while (remainingFacts.Any())
        {
            var remainingCapacity = Capacity - CurrentFactCount;

            _facts.AddRange(remainingFacts.Take(remainingCapacity));

            remainingFacts = remainingFacts
                .Skip(remainingCapacity)
                .ToList();

            if (CurrentFactCount == Capacity)
            {
                Write(
                    OutputPath,
                    _facts,
                    CurrentShardIndex
                );

                _facts.Clear();
                CurrentShardIndex++;
            }
        }
    }

    public void Flush()
    {
        if (_facts.Any())
        {
            Write(
                OutputPath,
                _facts,
                CurrentShardIndex == 0 ? null : CurrentShardIndex
            );

            CurrentShardIndex = 0;
            _facts.Clear();
        }
    }

    public static void Write(string outputPath, IEnumerable<Fact> facts, int? shard = null)
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

        var fileName = shard != null ? $"facts_{shard}.json" : "facts.json";
        var filePath = Path.Join(outputPath, fileName);

        Log.Information($"Wrote {facts.Count()} facts to {filePath}");

        Directory.CreateDirectory(outputPath);
        File.WriteAllText
        (
            filePath,
            GleanSerializer.Encode(output)
        );
    }
}
