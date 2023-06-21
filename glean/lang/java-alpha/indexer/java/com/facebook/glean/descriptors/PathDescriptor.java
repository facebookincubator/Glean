// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.schema.javakotlin_alpha.Name;
import com.facebook.glean.schema.javakotlin_alpha.Path;
import com.facebook.glean.schema.javakotlin_alpha.PathKey;
import javax.lang.model.element.TypeElement;

public class PathDescriptor {

  public static class NameAndPath {
    public Name simple;
    public Path path;

    NameAndPath(Name simple, Path path) {
      this.simple = simple;
      this.path = path;
    }
  }

  private static NameAndPath structureJavaName(IndexerContext ic, String javaFormattedName) {
    String[] expandedName = javaFormattedName.split("\\.");
    int length = expandedName.length;

    Name simple = NameDescriptor.describe(ic, expandedName[length - 1]);
    Path outer = null;
    for (int i = 0; i < length - 1; i++) {
      outer = PathDescriptor.describe(ic, expandedName[i], outer);
    }
    if (outer == null) {
      outer = PathDescriptor.describe(ic, "default", null);
    }

    return new NameAndPath(simple, outer);
  }

  public static NameAndPath structureTypeName(IndexerContext ic, TypeElement element) {

    javax.lang.model.element.Name fullyQualifiedName = element.getQualifiedName();
    return structureJavaName(ic, fullyQualifiedName.toString());
  }

  public static NameAndPath structurePath(IndexerContext ic, String name) {
    return structureJavaName(ic, name);
  }

  public static Path describe(IndexerContext ic, String name) {
    PathKey pathKey =
        new PathKey.Builder().setBase(NameDescriptor.describe(ic, name)).setContainer(null).build();
    Path _path = new Path.Builder().setKey(pathKey).build();
    ic.logger.indentedLog("Path: ");
    return _path;
  }

  public static Path describe(IndexerContext ic, String name, Path container) {
    PathKey pathKey =
        new PathKey.Builder()
            .setBase(NameDescriptor.describe(ic, name))
            .setContainer(container)
            .build();
    Path _path = new Path.Builder().setKey(pathKey).build();
    return _path;
  }

  public static Path describe(IndexerContext ic, Name name, Path container) {
    PathKey pathKey = new PathKey.Builder().setBase(name).setContainer(container).build();
    Path _path = new Path.Builder().setKey(pathKey).build();
    return _path;
  }
}
