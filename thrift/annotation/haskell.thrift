/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

include "thrift/annotation/scope.thrift"
include "thrift/annotation/thrift.thrift"

package "facebook.com/thrift/annotation/haskell"

namespace hs Facebook.Thrift.Annotation.Haskell

/**
 * Overrides the Haskell type used for a typedef, field, or function parameter.
 *
 * The `name` field specifies the target Haskell type. Supported values:
 *
 * * `"Int"` — use `Int` instead of `Int64` for `i64`
 * * `"String"` — use `String` instead of `Text` for `string`
 * * `"ByteString"` — use `ByteString` instead of `Text` for `string`
 * * `"HashMap"` — use `HashMap` instead of `Map` for `map`
 * * `"HashSet"` — use `HashSet` instead of `Set` for `set`
 * * `"Vector"` — use `Vector` instead of `[]` for `list`
 * * `"VectorStorable"` — use `Storable Vector` instead of `[]` for `list`
 *
 * For example:
 *
 *   @haskell.Type{name = "HashMap"}
 *   typedef map<string, string> MyMap;
 */
@scope.Typedef
@scope.Field
@scope.FunctionParameter
struct Type {
  1: string name;
}

/**
 * Makes struct fields lazy (non-strict). Can be applied to an entire struct
 * to set the default laziness for all fields, or to individual fields.
 * Field-level annotations override struct-level ones.
 *
 * For example:
 *
 *   @haskell.Lazy
 *   struct Foo {
 *     @haskell.Strict
 *     1: i32 strictField;   // strict despite struct-level @Lazy
 *     2: i32 lazyField;     // lazy from struct-level @Lazy
 *   }
 */
@scope.Struct
@scope.Field
struct Lazy {}

/**
 * Makes struct fields strict (eagerly evaluated). Can be applied to an
 * entire struct to set the default strictness for all fields, or to
 * individual fields. Field-level annotations override struct-level ones.
 *
 * For example:
 *
 *   struct Foo {
 *     @haskell.Strict
 *     1: i32 strictField;
 *     2: i32 defaultField;
 *   }
 */
@scope.Struct
@scope.Field
struct Strict {}

/**
 * Excludes a field from the generated Haskell record. The field will not
 * appear in the generated data type or serialization code.
 *
 * For example:
 *
 *   struct Foo {
 *     @haskell.Hidden
 *     1: i64 internalField;
 *     2: i32 visibleField;
 *   }
 */
@scope.Field
struct Hidden {}

/**
 * Generates a Haskell `newtype` wrapper instead of a `type` alias for a
 * typedef. This creates a distinct type that is not automatically
 * interchangeable with its underlying type.
 *
 * For example:
 *
 *   @haskell.Newtype
 *   typedef i64 UserId;
 *
 * Generates `newtype UserId = UserId Int64` instead of `type UserId = Int64`.
 */
@scope.Typedef
struct Newtype {}

/**
 * Indicates that a union type has no empty variant. By default, unions
 * generate an empty constructor (for the case where no field is set).
 * This annotation removes it, making the union non-empty.
 *
 * For example:
 *
 *   @haskell.NonEmpty
 *   union Result {
 *     1: string success;
 *     2: string error;
 *   }
 */
@scope.Union
struct NonEmpty {}

/**
 * Generates an enum without an unknown/catch-all variant. By default,
 * enums include a catch-all constructor for unrecognised values. This
 * annotation removes it, treating the enum as a closed sum type.
 *
 * For example:
 *
 *   @haskell.NoUnknown
 *   enum Color {
 *     RED = 1,
 *     GREEN = 2,
 *     BLUE = 3,
 *   }
 */
@scope.Enum
struct NoUnknown {}

/**
 * Generates an enum as a newtype wrapper around an integer instead of a
 * Haskell sum type. This can be more efficient for enums with many
 * alternatives.
 *
 * The optional `value` field controls the representation:
 * * omitted — uses a plain integer newtype
 * * `"thriftenum"` — uses a Thrift-compatible enum encoding
 *
 * For example:
 *
 *   @haskell.PseudoEnum
 *   enum Status {
 *     ACTIVE = 1,
 *     INACTIVE = 2,
 *   }
 *
 *   @haskell.PseudoEnum{value = "thriftenum"}
 *   enum Priority {
 *     LOW = 1,
 *     HIGH = 2,
 *   }
 */
@scope.Enum
struct PseudoEnum {
  1: string value;
}

/**
 * Overrides the default prefix used for generated field names, enum
 * alternatives, union alternatives, or function names. By default, the
 * Thrift compiler generates prefixes based on the parent type name to
 * avoid name collisions.
 *
 * For example:
 *
 *   @haskell.Prefix{name = "ps_"}
 *   struct PrefixedStruct {
 *     1: i64 foo;    // generates ps_foo
 *     2: i32 bar;    // generates ps_bar
 *   }
 */
@scope.Struct
@scope.Union
@scope.Enum
@scope.Function
struct Prefix {
  1: string name;
}
