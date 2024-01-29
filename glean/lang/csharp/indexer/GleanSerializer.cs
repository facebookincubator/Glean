/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Indexer.Schema;
using Microsoft.CodeAnalysis;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Serialization;
using System;
using System.Linq;
using System.Reflection;

namespace Glean.Indexer;

public class GleanSerializer
{
    private static readonly JsonSerializerSettings DefaultSettings = new JsonSerializerSettings()
    {
        NullValueHandling = NullValueHandling.Ignore,
        ContractResolver = new GleanContractResolver(),
        Converters = {new GleanEnumConverter()}
    };

    private static readonly JsonSerializer DefaultSerializer = JsonSerializer.Create(DefaultSettings);

    public static string Encode(object @object, Formatting formatting = Formatting.None)
    {
        return Serialize(@object).ToString(formatting);
    }

    public static JToken Serialize(object @object)
    {
        return JToken.FromObject(@object, DefaultSerializer);
    }
}

public class GleanContractResolver : DefaultContractResolver
{
    public GleanContractResolver()
    {
        NamingStrategy = new CamelCaseNamingStrategy();
    }

    protected override JsonObjectContract CreateObjectContract(Type objectType)
    {
        JsonObjectContract contract = base.CreateObjectContract(objectType);

        if (typeof(IFactWithKey).IsAssignableFrom(objectType))
        {
            contract.Converter = new GleanFactConverter();
        }

        return contract;
    }
}

class GleanFactConverter : JsonConverter
{
    public override bool CanRead => false;

    public override bool CanConvert(Type objectType)
    {
        return typeof(IFactWithKey).IsAssignableFrom(objectType);
    }

    public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
    {
        if (value is IFactWithKey fact)
        {
            GleanSerializer.Serialize(new { Key = fact.UntypedKey }).WriteTo(writer);
        }
    }

    public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
    {
        throw new NotImplementedException();
    }
}

class GleanEnumConverter : JsonConverter
{
    public override bool CanRead => false;

    public override bool CanConvert(Type objectType)
    {
        return objectType.IsEnum;
    }

    public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
    {
        var names = value
            .GetType()
            .GetFields(BindingFlags.Public | BindingFlags.Static)
            .Select(f => f.Name)
            .ToArray();

        var index = Array.IndexOf(names, value.ToString());

        writer.WriteValue(index);
    }

    public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
    {
        throw new NotImplementedException();
    }
}
