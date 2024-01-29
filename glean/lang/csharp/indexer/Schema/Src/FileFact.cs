// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

namespace Indexer.Schema.Src;

public record FileFact(string Path) : FactWithKey<string>(Predicate.File, Key: Path);
