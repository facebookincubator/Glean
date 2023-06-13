// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.generics;

import java.util.List;

public class Main {

  private Map<String, List<String>> iMap = new Map();

  public List<String> thisReturnsAList() {
    return null;
  }

  public static void main(String[] args) {
    Map<String, Main> map = new Map();
    Main main = new Main();
    String key = "key";
    map.put(key, main);
    map.get(key);
  }
}
