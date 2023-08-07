// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.glean;

/*
  Comment for Glean class
*/
@Deprecated
public class Glean {

  public static final String LOG_TAG = "Glean";

  @Deprecated
  @SuppressWarnings("deprecation")
  public POJO fetch() {
    return new POJO();
  }

  public static void main(String[] args) {
    Glean glean = new Glean();
    POJO pojo = glean.fetch();
    pojo.getE();
    pojo.getE(10);
    pojo.getList(null);
  }
}
