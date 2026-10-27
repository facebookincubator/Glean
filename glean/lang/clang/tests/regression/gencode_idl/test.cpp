/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Regression coverage for Glean docblock decoding in the C++ indexer.
//
// The Thrift decoder must ignore annotations tagged with an "idl" (e.g. Ligen
// emits `Glean{"idl":"ligen",...}`) and must not crash on an unrecognized
// "kind". Previously Ligen's "class"/"method" kinds reached `kind_map.at()`,
// which threw std::out_of_range ("map::at") and aborted indexing of the whole
// translation unit (0 facts).

struct Foo {
  // Thrift annotation (no "idl"): decoded normally, emits a CxxToThrift fact.
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "do_it"} */
  virtual void generated_from_thrift();

  // Ligen annotation: skipped by the Thrift decoder (Ligen has its own indexer).
  /** Glean{"class":"Foo","file":"some/mod.ligen","idl":"ligen","kind":"method","name":"ligen_method"} */
  virtual void ligen_method();

  // Unrecognized Thrift kind (no "idl"): skipped with a warning, not a crash.
  /** Glean {"file": "some/module.thrift", "name": "FutureKind", "kind": "interaction"} */
  virtual void unknown_kind();

  // Newline between "Glean" and the brace: not an annotation, skipped.
  /** Glean
   * {"file": "some/module.thrift", "service": "MyService", "function": "newline_skipped"} */
  virtual void newline_skipped();

  // Two annotations in one comment, Thrift second: only the Thrift one decodes.
  /** Glean {"idl": "ligen", "kind": "method", "name": "ligen_first"} Glean {"file": "some/module.thrift", "service": "MyService", "function": "thrift_second"} */
  virtual void thrift_second();

  // Nested object with a brace inside a string: one annotation, decoded.
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "nested_object", "extra": {"note": "a } in a string", "list": [{"x": 1}]}} */
  virtual void nested_object();

  // Annotation spanning two lines: decoded up to its matching brace.
  /** Glean {"file": "some/module.thrift", "service": "MyService",
       "function": "multi_line"} */
  virtual void multi_line();
};

// Ligen class annotation at namespace scope: also skipped.
/** Glean{"file":"some/mod.ligen","idl":"ligen","kind":"class","name":"LigenClass"} */
struct LigenClass {};
