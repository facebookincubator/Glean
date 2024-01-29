/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System.Linq;

namespace Glean.Indexer.Tests;

[TestFixture]
public class GleanSerializerTests
{
    [Test]
    public void TestSerializeNestedFact()
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
    public void TestSerializeFactCollection()
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
    public void TestSerializeSumType()
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
    public void TestSerializeEnum()
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
}
