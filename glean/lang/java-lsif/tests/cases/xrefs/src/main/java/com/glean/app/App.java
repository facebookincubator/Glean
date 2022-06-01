/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.glean.app;

/** Hello world! */
public class App {

  private final String message = "Hello World!";

  public App() {}

  public static void main(String[] args) {
    System.out.println(new App().getMessage());
  }

  private final String getMessage() {
    return message;
  }
}
