// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.logger;

public class BufferedLogger implements ILogger {

  private final StringBuilder buffer = new StringBuilder();

  @Override
  public void log(final String msg) {
    buffer.append(msg);
    buffer.append("\n");
  }

  @Override
  public String toString() {
    return buffer.toString();
  }
}
