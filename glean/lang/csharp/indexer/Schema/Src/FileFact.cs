/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace Glean.Indexer.Schema.Src;

public record FileFact(string Path) : FactWithKey<string>(Predicate.File, Key: Path);
