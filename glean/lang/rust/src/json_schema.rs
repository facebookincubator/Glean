/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// rust-glean/src/json_schema.rs

use std::path::PathBuf;

use serde::Deserialize;

#[derive(Clone, Debug, Deserialize)]
pub struct Root {
    pub impls: Vec<Impl>,
    pub defs: Vec<Def>,
    pub refs: Vec<Ref>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Impl {
    pub kind: ImplKind,
    pub id: u32,
    pub children: Vec<RefId>,
    pub span: Span,
}

#[derive(Clone, Debug, Deserialize)]
pub enum ImplKind {
    Inherent,
    Direct,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Def {
    pub kind: DefKind,
    pub qualname: String,
    // Despite the name, this is actually the type...
    // TODO(pcwalton): How do we parse this?
    pub value: String,
    pub span: Span,
    pub id: RefId,
}

#[derive(Clone, Debug, Deserialize)]
pub enum DefKind {
    Const,
    Enum,
    Field,
    ForeignFunction,
    ForeignStatic,
    Function,
    Local,
    Method,
    Mod,
    Static,
    Struct,
    StructVariant,
    Trait,
    TupleVariant,
    Type,
    Union,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Ref {
    pub kind: RefKind,
    pub ref_id: RefId,
    pub span: Span,
}

#[derive(Clone, Debug, Deserialize)]
pub enum RefKind {
    Mod,
    Type,
    // Includes constants, I think.
    Variable,
    Function,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RefId {
    pub index: u32,
    pub krate: u32,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Span {
    pub byte_start: u64,
    pub byte_end: u64,
    pub column_start: u64,
    pub column_end: u64,
    pub line_start: u64,
    pub line_end: u64,
    pub file_name: PathBuf,
}
