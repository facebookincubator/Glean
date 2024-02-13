/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System.IO;
using Glean.Discovery;
using Glean.Indexer.Schema.Src;

using UnevaluatedProject = Microsoft.Build.Construction.ProjectRootElement;
using EvaluatedProject = Microsoft.Build.Evaluation.Project;

namespace Glean.Indexer.Schema.CSharp;

public record struct ProjectFactKey
    ( ProjectSource Source
    , string? PlatformTarget
    , string? TargetFramework
    , string? Sdk
    , string? OutputType
    , string? AssemblyName
    , string? RootNamespace
    );

public record ProjectFact(ProjectFactKey Key) : FactWithKey<ProjectFactKey>(Predicate.Project, Key)
{
    public ProjectFact
        ( ProjectSource projectSource
        , UnevaluatedProject unevaluatedProject
        , EvaluatedProject evaluatedProject
        ) : this(new ProjectFactKey
            ( projectSource
            , OmitIfNullOrWhitespace(evaluatedProject.GetPropertyValue("PlatformTarget"))
            , OmitIfNullOrWhitespace(evaluatedProject.GetPropertyValue("TargetFramework"))
            , OmitIfNullOrWhitespace(unevaluatedProject.Sdk)
            , OmitIfNullOrWhitespace(evaluatedProject.GetPropertyValue("OutputType"))
            , OmitIfNullOrWhitespace(evaluatedProject.GetPropertyValue("AssemblyName"))
            , OmitIfNullOrWhitespace(evaluatedProject.GetPropertyValue("RootNamespace"))
            )) {}

    private static string? OmitIfNullOrWhitespace(string? s) => string.IsNullOrWhiteSpace(s) ? default : s;
}
