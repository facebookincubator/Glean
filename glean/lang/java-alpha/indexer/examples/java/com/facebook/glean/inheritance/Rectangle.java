// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.inheritance;

public class Rectangle extends Drawable implements IPolygon, IX {

  private final String color;

  public Rectangle(final String name, final String color) {
    super(name);
    this.color = color;
  }

  public void paint() {
    super.draw();
  }

  @Override
  public int getNumSides() {
    return 4;
  }

  @Override
  public void squawkA() {
    System.out.println("A");
  }

  @Override
  public void squawkB() {
    System.out.println("B");
  }

  @Override
  public void squawkX() {
    System.out.println("X");
  }
}
