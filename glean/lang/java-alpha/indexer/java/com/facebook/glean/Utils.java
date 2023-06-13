// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean;

public class Utils {
  private static final String FBSOURCE = "fbsource";

  public static String normalizePath(String path) {
    // Paths may contain fbsource more than once, fbsource-, or, number dash directory
    int fbSourceStartIndex = path.indexOf(FBSOURCE);
    String pathWithoutFbSource = path;
    while (fbSourceStartIndex != -1) {
      pathWithoutFbSource = pathWithoutFbSource.substring(fbSourceStartIndex + FBSOURCE.length());
      fbSourceStartIndex = pathWithoutFbSource.indexOf(FBSOURCE);
    }

    String[] splitPath = pathWithoutFbSource.split("[0-9]*-[0-9]*");
    String cleanPath =
        splitPath.length == 1
            ? splitPath[0]
            : (splitPath.length < 1) ? path : String.join("", splitPath);
    String finalPath =
        cleanPath.startsWith(FBSOURCE)
            ? cleanPath
            : FBSOURCE + (cleanPath.startsWith("//") ? cleanPath.substring(1) : cleanPath);

    return finalPath;
  }
}
