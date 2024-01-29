// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Indexer;
using Indexer.Schema;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.MSBuild;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using NUnit.Framework;
using System.Linq;

namespace Indexer.Test;

[TestFixture]
public class IndexerTests
{
    string sampleProjectPath = null;
    Compilation compilation = null;

    [OneTimeSetUp]
    public void SetUp()
    {
        Build.Initialize();

        this.sampleProjectPath = Path.Combine
        (
            Hg.RepoRoot,
            Environment.GetEnvironmentVariable("SAMPLE_PROJECT_PATH")
        );

        using (var workspace = MSBuildWorkspace.Create())
        {
            this.compilation = Build.CompileProject(workspace, this.sampleProjectPath);
        }
    }

    [OneTimeTearDown]
    public void TearDown()
    {

    }

    [Test]
    public void CanIndexSampleProject()
    {
        var facts = Indexer.IndexProject(this.sampleProjectPath);
        Assert.NotNull(facts);
    }

    [Test]
    public void SerializeNestedFact()
    {
        var outerFieldKey = "foo";
        var innerFieldKey = "bar";
        var key = new ArbitraryFact2Key(outerFieldKey, new ArbitraryFact1(innerFieldKey));

        var expected = new JObject
        {
            ["key"] = new JObject
            {
                ["field1"] = outerFieldKey,
                ["field2"] = new JObject
                {
                    ["key"] = innerFieldKey
                }
            }
        };
        var actual = new ArbitraryFact2(key);
        Assert.AreEqual
        (
            GleanSerializer.Encode(expected),
            GleanSerializer.Encode(actual)
        );
    }

    [Test]
    public void SerializeFactCollection()
    {
        var innerFieldKey = "bar";
        var innerFact = new ArbitraryFact1(innerFieldKey);
        var outerFact = new ArbitraryFact3(new [] {innerFact});

        var expected = new JObject
        {
            ["key"] = new JArray()
            {
                new JObject
                {
                    ["key"] = innerFieldKey,
                }
            }
        };
        var actual = outerFact;
        Assert.AreEqual
        (
            GleanSerializer.Encode(expected),
            GleanSerializer.Encode(actual)
        );
    }

    [Test]
    public void SerializeSumType()
    {
        var outerFieldKey = "foo";
        var innerFieldKey = "bar";

        var fact1 = new ArbitraryFact1(innerFieldKey);
        var fact2 = new ArbitraryFact2(new ArbitraryFact2Key(outerFieldKey, fact1));

        var expected = new JObject
        {
            ["option1"] = new JObject
            {
                ["key"] = innerFieldKey,
            }
        };
        var actual = new ArbitrarySumType(Option1: fact1);
        Assert.AreEqual
        (
            GleanSerializer.Encode(expected),
            GleanSerializer.Encode(actual)
        );

        expected = new JObject
        {
            ["option2"] = new JObject
            {
                ["key"] = new JObject
                {
                    ["field1"] = outerFieldKey,
                    ["field2"] = new JObject
                    {
                        ["key"] = innerFieldKey
                    }
                }
            }
        };
        actual = new ArbitrarySumType(Option2: fact2);
        Assert.AreEqual
        (
            GleanSerializer.Encode(expected),
            GleanSerializer.Encode(actual)
        );
    }

    [Test]
    public void SerializeEnum()
    {
        var values = new [] { ArbitraryEnum.A, ArbitraryEnum.B, ArbitraryEnum.C };
        var fact = new ArbitraryFact4(values);

        var expected = new JObject
        {
            ["key"] = new JArray() {0,1,2}
        };
        var actual = fact;
        Assert.AreEqual
        (
            GleanSerializer.Encode(expected),
            GleanSerializer.Encode(actual)
        );
    }

    [Test]
    public void TestCleanFileName()
    {
        string input = "invalid:file|name";
        string expected = "invalid_file_name";
        Assert.AreEqual(expected, FactFile.CleanFileName(input));
    }

    [Test]
    public void TestDisambiguateFileName()
    {
        string[] parts = { "dir1", "dir2", "file" };
        string expected1 = "file";
        Assert.AreEqual(expected1, FactFile.DisambiguateFileName(parts, 2));

        // Parent directory included to disambiguate
        string expected2 = "dir2_file";
        Assert.AreEqual(expected2, FactFile.DisambiguateFileName(parts, 2));
    }

    [Test]
    public void TestGetFactFileNameForProject()
    {
        string path1 = @"dir1/dir2/solution.sln";
        string expected1 = @"solution.sln.json";
        Assert.AreEqual(expected1, FactFile.GetFactFileNameForProject(path1));

        // Parent directory included to disambiguate
        string path2 = @"dir1/dir3/solution.sln";
        string expected2 = @"dir3_solution.sln.json";
        Assert.AreEqual(expected2, FactFile.GetFactFileNameForProject(path2));
    }
}
