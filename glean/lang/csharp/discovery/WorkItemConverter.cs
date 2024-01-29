/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Glean.Discovery;

public class WorkItemConverter : JsonConverter<WorkItem>
{
    public override WorkItem Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        using (var document = JsonDocument.ParseValue(ref reader))
        {
            var root = document.RootElement.Clone();
            var type = Enum.Parse<WorkType>(root.GetProperty("Type").GetString());
            var text = root.GetRawText();

            return type switch
            {
                WorkType.MSBuildProject
                    => JsonSerializer.Deserialize<WorkItem.MSBuildProject>(text),
                WorkType.MSBuildSolution
                    => JsonSerializer.Deserialize<WorkItem.MSBuildSolution>(text),
                WorkType.UnityPackage
                    => JsonSerializer.Deserialize<WorkItem.UnityPackage>(text),
                WorkType.BuckBuildTarget
                    => JsonSerializer.Deserialize<WorkItem.BuckBuildTarget>(text),

                _ => throw new InvalidOperationException($"Invalid work type: {type}")
            };
        }
    }

    public override void Write(Utf8JsonWriter writer, WorkItem value, JsonSerializerOptions options)
    {
        var type = value.Type switch
        {
            WorkType.MSBuildProject
                => typeof(WorkItem.MSBuildProject),
            WorkType.MSBuildSolution
                => typeof(WorkItem.MSBuildSolution),
            WorkType.UnityPackage
                => typeof(WorkItem.UnityPackage),
            WorkType.BuckBuildTarget
                => typeof(WorkItem.BuckBuildTarget),

            _ => throw new InvalidOperationException($"Invalid work type: {value.Type}")
        };

        JsonSerializer.Serialize(writer, value, type, options);
    }
}
