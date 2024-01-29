/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace Glean.Indexer.Schema.CSharp;

public record struct ObjectCreationLocationFactKey
    ( Type Type
    , MethodFact Constructor
    , Location Location
    );

public record ObjectCreationLocationFact(ObjectCreationLocationFactKey Key)
    : FactWithKey<ObjectCreationLocationFactKey>(Predicate.ObjectCreationLocation, Key);
