// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean;

import java.util.regex.*;

public class Utils {
  // e.g. to match 'fbsource/147-16788900/fbandroid/*'
  private static final Pattern NOISE = Pattern.compile("^[0-9]+-[0-9]+/");

  // to match any remnant repo anchors
  // e.g.
  // '/data/sandcastle/boxes/eden-trunk-hg-fbcode-fbsource/''
  // or
  // '/data/sandcastle/boxes/fbsource/'
  //
  private static final Pattern FBSOURCE = Pattern.compile("^/.*[/-]fbsource/");

  // what we need is to pass an indexerRoot through...

  /** Input paths are absolute, and need to be made relative and de-noised */
  public static String normalizePath(String path) {
    Matcher n = FBSOURCE.matcher(path);
    String cleanPath = n.replaceFirst("");
    Matcher m = NOISE.matcher(cleanPath);
    return m.replaceFirst("");
  }
}
