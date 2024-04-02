/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Foo {

  /** Glean {"file": "thrift/compiler/test/fixtures/inheritance/src/module.thrift", "service": "MyRoot", "function": "do_root"} */
  virtual void generated_from_thrift();

  /** Glean {"file": "thri BROKEN JSON} */
  virtual void not_json();

  /** Glean {"service": "BadInteraction", "function": "foo"} */
  virtual void missing_value();

  /** Glean {"file": 12, "service": "BadInteraction", "function": "foo"} */
  virtual void wrong_types();

  /**
   * Glean {"file": 12, "service": "BadInteraction", "function": "foo"}
   */
  virtual void generated_from_thrift_2();

};
