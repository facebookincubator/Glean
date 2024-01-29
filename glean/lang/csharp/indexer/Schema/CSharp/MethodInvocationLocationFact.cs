/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace Glean.Indexer.Schema.CSharp;

public record struct MethodInvocationLocationFactKey
    ( MethodFact Method
    , Location Location
    );

public record MethodInvocationLocationFact(MethodInvocationLocationFactKey Key)
    : FactWithKey<MethodInvocationLocationFactKey>(Predicate.MethodInvocationLocation, Key);
