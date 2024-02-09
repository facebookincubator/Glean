/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Newtonsoft.Json;

namespace Glean.Indexer.Schema.CSharp;

public record ProjectSource(UnityProjectSourceFact? UnityProjectSource = null, MSBuildProjectSourceFact? MsbuildProjectSource = null)
{
    public static ProjectSource MSBuild(MSBuildProjectSourceFact msbuildProjectSource) => new (null, msbuildProjectSource);
    public static ProjectSource Unity(UnityProjectSourceFact unityProjectSource) => new (unityProjectSource, null);
}
