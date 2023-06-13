// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.glean;

public class POJO {
  public static final int a = 1;
  @Deprecated private static final int b = 2;
  protected static final int c = 3;
  static final int d = 4;

  public String e = "e";
  private String f = "f";
  protected String g = "g";
  String h = "h";

  public boolean[][][] i = new boolean[5][5][5];
  public java.util.List aList = null;

  @Deprecated
  public POJO() {
    this("eDefaultCtor", "fDefaultCtor", "gDefaultCtor", "hDefaultCtor");
  }

  public POJO(String e, String f, String g, String h) {
    this.e = e;
    this.f = f;
    this.g = g;
    this.h = h;
  }

  public void getList(java.util.List<? extends java.lang.Class> clazz) {
    int x = 10;
    return;
  }

  public String getE() {
    return e;
  }

  public String getE(final int randomArg) {
    return e;
  }

  public boolean isInstanceOfList(Object object) {
    return object instanceof java.util.List;
  }
}
