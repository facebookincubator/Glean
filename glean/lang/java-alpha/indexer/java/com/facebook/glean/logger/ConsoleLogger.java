// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.logger;

public class ConsoleLogger implements ILogger {

  @Override
  public void log(final String msg) {
    System.err.println(msg);
  }
}
