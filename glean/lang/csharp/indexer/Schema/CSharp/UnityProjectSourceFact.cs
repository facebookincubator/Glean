/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Facebook.SocialVR.Packages;

using Glean.Indexer.Schema;
using Glean.Indexer.Schema.Src;

namespace Glean.Indexer.Schema.CSharp;

public record struct UnityProjectSourceFactKey
    ( string ProjectBasename
    , UnityPackageFact UnityPackage
    , AssemblyDefinition.Type AssemblyType
    , FileFact ProjectTemplate
    );

public record UnityProjectSourceFact(UnityProjectSourceFactKey Key) : FactWithKey<UnityProjectSourceFactKey>(Predicate.UnityProjectSource, Key);
