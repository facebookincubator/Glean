/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.dex;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.EnumSet;
import java.util.Set;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Tests for {@link D8Utils#moveSingleDexOutput}. */
public class D8UtilsTest {

  @Rule public TemporaryFolder tmp = new TemporaryFolder();

  private static final Set<D8Options> FAIL_ON_MULTIPLE = EnumSet.of(D8Options.FAIL_ON_MULTIPLE_DEX);

  private static final Set<D8Options> NO_FAIL = EnumSet.noneOf(D8Options.class);

  @Test
  public void testMoveSingleDexOutput_singleFile() throws Exception {
    Path d8OutputDir = tmp.newFolder("d8-output").toPath();
    Files.write(d8OutputDir.resolve("classes.dex"), new byte[] {1, 2, 3});

    Path dest = tmp.getRoot().toPath().resolve("final.dex");
    D8Utils.moveSingleDexOutput(d8OutputDir, dest, FAIL_ON_MULTIPLE);

    assertTrue("Destination file should exist", Files.exists(dest));
    assertEquals("File content should match", 3, Files.size(dest));
  }

  @Test
  public void testMoveSingleDexOutput_emptyDir() throws Exception {
    Path d8OutputDir = tmp.newFolder("d8-empty").toPath();
    Path dest = tmp.getRoot().toPath().resolve("final.dex");

    D8Utils.moveSingleDexOutput(d8OutputDir, dest, FAIL_ON_MULTIPLE);

    assertFalse("Destination should not exist for empty input", Files.exists(dest));
  }

  @Test
  public void testMoveSingleDexOutput_multipleFiles_throwsWithAllFilenames() throws Exception {
    Path d8OutputDir = tmp.newFolder("d8-multi").toPath();
    Files.write(d8OutputDir.resolve("classes.dex"), new byte[] {1});
    Files.write(d8OutputDir.resolve("classes2.dex"), new byte[] {2});
    Files.write(d8OutputDir.resolve("classes3.dex"), new byte[] {3});

    Path dest = tmp.getRoot().toPath().resolve("final.dex");

    try {
      D8Utils.moveSingleDexOutput(d8OutputDir, dest, FAIL_ON_MULTIPLE);
      fail("Expected IllegalStateException for multiple DEX files");
    } catch (IllegalStateException e) {
      String msg = e.getMessage();
      assertTrue("Error should mention file count: " + msg, msg.contains("3 DEX files"));
      assertTrue("Error should list classes.dex: " + msg, msg.contains("classes.dex"));
      assertTrue("Error should list classes2.dex: " + msg, msg.contains("classes2.dex"));
      assertTrue("Error should list classes3.dex: " + msg, msg.contains("classes3.dex"));
      assertTrue("Error should suggest split_dex: " + msg, msg.contains("split_dex"));
    }
    assertFalse("Destination should not be written on error", Files.exists(dest));
  }

  @Test
  public void testMoveSingleDexOutput_twoFiles_throwsWithSplitDexHint() throws Exception {
    Path d8OutputDir = tmp.newFolder("d8-two").toPath();
    Files.write(d8OutputDir.resolve("classes.dex"), new byte[] {1});
    Files.write(d8OutputDir.resolve("classes2.dex"), new byte[] {2});

    Path dest = tmp.getRoot().toPath().resolve("final.dex");

    try {
      D8Utils.moveSingleDexOutput(d8OutputDir, dest, FAIL_ON_MULTIPLE);
      fail("Expected IllegalStateException for multiple DEX files");
    } catch (IllegalStateException e) {
      assertTrue(
          "Error should suggest use_split_dex: " + e.getMessage(),
          e.getMessage().contains("use_split_dex = True"));
    }
  }

  @Test
  public void testMoveSingleDexOutput_multipleFiles_noFailOption_movesFirstFile() throws Exception {
    Path d8OutputDir = tmp.newFolder("d8-multi-nofail").toPath();
    Files.write(d8OutputDir.resolve("classes.dex"), new byte[] {1, 2, 3});
    Files.write(d8OutputDir.resolve("classes2.dex"), new byte[] {4, 5});

    Path dest = tmp.getRoot().toPath().resolve("final.dex");
    D8Utils.moveSingleDexOutput(d8OutputDir, dest, NO_FAIL);

    assertTrue("Destination file should exist", Files.exists(dest));
    assertEquals("Should move first file (classes.dex)", 3, Files.size(dest));
  }
}
