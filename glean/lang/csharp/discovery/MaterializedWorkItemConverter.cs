/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System;
using Facebook.SocialVR.Packages;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Glean.Discovery;

public class MaterializedWorkItemConverter : JsonConverter<MaterializedWorkItem>
{
    public override MaterializedWorkItem Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        using (var document = JsonDocument.ParseValue(ref reader))
        {
            var root = document.RootElement.Clone();
            var type = Enum.Parse<WorkType>(root.GetProperty("Type").GetString());
            var text = root.GetRawText();

            return type switch
            {
                WorkType.MSBuildProject
                    => JsonSerializer.Deserialize<MaterializedWorkItem.MSBuildProject>(text),
                WorkType.MSBuildSolution
                    => JsonSerializer.Deserialize<MaterializedWorkItem.MSBuildSolution>(text),
                WorkType.UnityPackage
                    => JsonSerializer.Deserialize<MaterializedWorkItem.UnityPackage>(text),
                WorkType.BuckBuildTarget
                    => JsonSerializer.Deserialize<MaterializedWorkItem.BuckBuildTarget>(text),

                _ => throw new InvalidOperationException($"Invalid work type: {type}")
            };
        }
    }

    public override void Write(Utf8JsonWriter writer, MaterializedWorkItem value, JsonSerializerOptions options)
    {
        var type = value.Type switch
        {
            WorkType.MSBuildProject
                => typeof(MaterializedWorkItem.MSBuildProject),
            WorkType.MSBuildSolution
                => typeof(MaterializedWorkItem.MSBuildSolution),
            WorkType.UnityPackage
                => typeof(MaterializedWorkItem.UnityPackage),
            WorkType.BuckBuildTarget
                => typeof(MaterializedWorkItem.BuckBuildTarget),

            _ => throw new InvalidOperationException($"Invalid work type: {value.Type}")
        };

        JsonSerializer.Serialize(writer, value, type, options);
    }
}
