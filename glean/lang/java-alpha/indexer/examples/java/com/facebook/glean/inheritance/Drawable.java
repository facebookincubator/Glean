// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.inheritance;

public abstract class Drawable {
  protected final String name;

  public Drawable(final String name) {
    this.name = name;
  }

  public void draw() {
    // some implementation
  }
}
