/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System.IO;
using System.Linq;
using System.Text.Json;

namespace Glean.Indexer.Tests;

[TestFixture]
public class FactStoreTests
{
    private FactStore _factStore;

    private readonly string _outputPath = Directory.CreateTempSubdirectory().FullName;
    private const int _capacity = 5;

    [SetUp]
    public void SetUp()
    {
        _factStore = new FactStore(_capacity, _outputPath);
        Directory.CreateDirectory(_outputPath);
    }

    private int GetNumberOfFactFilesWritten()
    {
        return Directory.Exists(_outputPath)
            ? new DirectoryInfo(_outputPath)
                .EnumerateFileSystemInfos("*.json", SearchOption.AllDirectories)
                .Count()
            : 0;
    }

    private IEnumerable<int> GetFactCounts()
    {
        var factFiles = Directory.GetFiles(_outputPath, "*.json");
        foreach (var file in factFiles)
        {
            var encodedFacts = File.ReadAllText(file);
            var document = JsonDocument.Parse(encodedFacts);
            var root = document.RootElement;
            if (root.ValueKind == JsonValueKind.Array)
            {
                int fileFactCount = 0;
                foreach (var item in root.EnumerateArray())
                {
                    if (item.TryGetProperty("facts", out var factsProperty))
                    {
                        if (factsProperty.ValueKind == JsonValueKind.Array)
                        {
                            fileFactCount += factsProperty.GetArrayLength();
                        }
                    }
                }

                yield return fileFactCount;
            }
        }
    }

    [TearDown]
    public void TearDown()
    {
        Directory.Delete(_outputPath, true);
    }

    [Test]
    public void TestAddRangeGreaterThanDoubleCapacity()
    {
        var factCount = (_capacity * 2) + 1;
        var facts = Enumerable.Repeat(new ArbitraryFact1(string.Empty), factCount);
        _factStore.AddRange(facts);

        Assert.AreEqual(factCount % _capacity, _factStore.CurrentFactCount);
        Assert.AreEqual(2, _factStore.CurrentShardIndex);

        Assert.AreEqual(2, GetNumberOfFactFilesWritten());
        _factStore.Flush();
        Assert.AreEqual(3, GetNumberOfFactFilesWritten());

        var factCounts = GetFactCounts();
        Assert.AreEqual(factCounts.Count(n => n == _capacity), factCount / _capacity);
        Assert.AreEqual(factCounts.Count(n => n == factCount % _capacity), 1);
    }

    [Test]
    public void TestAddRangeGreaterThanCapacity()
    {
        var factCount = _capacity + 1;
        var facts = Enumerable.Repeat(new ArbitraryFact1(string.Empty), factCount);
        _factStore.AddRange(facts);

        Assert.AreEqual(factCount % _capacity, _factStore.CurrentFactCount);
        Assert.AreEqual(1, _factStore.CurrentShardIndex);

        Assert.AreEqual(1, GetNumberOfFactFilesWritten());
        _factStore.Flush();
        Assert.AreEqual(2, GetNumberOfFactFilesWritten());

        var factCounts = GetFactCounts();
        Assert.AreEqual(factCounts.Count(n => n == _capacity), factCount / _capacity);
        Assert.AreEqual(factCounts.Count(n => n == factCount % _capacity), 1);
    }

    [Test]
    public void TestAddRangeEqualToCapacity()
    {
        var factCount = _capacity;
        var facts = Enumerable.Repeat(new ArbitraryFact1(string.Empty), factCount);
        _factStore.AddRange(facts);

        Assert.AreEqual(factCount % _capacity, _factStore.CurrentFactCount);
        Assert.AreEqual(1, _factStore.CurrentShardIndex);

        Assert.AreEqual(1, GetNumberOfFactFilesWritten());
        _factStore.Flush();
        Assert.AreEqual(1, GetNumberOfFactFilesWritten());

        var factCounts = GetFactCounts();
        Assert.True(factCounts.All(n => n == factCount));
    }

    [Test]
    public void TestAddRangeLessThanCapacity()
    {
        var factCount = _capacity - 1;
        var facts = Enumerable.Repeat(new ArbitraryFact1(string.Empty), factCount);
        _factStore.AddRange(facts);

        Assert.AreEqual(factCount % _capacity, _factStore.CurrentFactCount);
        Assert.AreEqual(0, _factStore.CurrentShardIndex);

        Assert.AreEqual(0, GetNumberOfFactFilesWritten());
        _factStore.Flush();
        Assert.AreEqual(1, GetNumberOfFactFilesWritten());

        var factCounts = GetFactCounts();
        Assert.True(factCounts.All(n => n == factCount));
    }
}
