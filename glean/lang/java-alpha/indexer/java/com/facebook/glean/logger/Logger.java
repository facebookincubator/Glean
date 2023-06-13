// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.logger;

import java.util.ArrayList;
import java.util.List;

public class Logger {
  private boolean enabled = false;
  private int indentLevel = 0;
  private String stringPad = getStringPad(indentLevel);
  private final List<ILogger> loggers = new ArrayList<>();

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public void addLogger(ILogger logger) {
    loggers.add(logger);
  }

  public void increaseIndent() {
    if (!enabled) {
      return;
    }

    indentLevel += 1;
    stringPad = getStringPad(indentLevel);
  }

  public void decreaseIndent() {
    if (!enabled) {
      return;
    }

    indentLevel -= 1;
    if (indentLevel < 0) {
      throw new RuntimeException("Indent level cannot be negative");
    }
    stringPad = getStringPad(indentLevel);
  }

  public void indentedLog(String msg) {
    log(stringPad + msg);
  }

  public void log(String msg) {
    if (!enabled) {
      return;
    }

    for (ILogger iLogger : loggers) {
      iLogger.log(msg);
    }
  }

  private String getStringPad(int _indentLevel) {
    return new String(new char[_indentLevel]).replace("\0", "  ");
  }
}
