/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

import fs from 'fs-extra';
import os from 'os';
import path from 'path';
import shell from 'shelljs';

class Git {
  constructor(private dir: string) {
    const res = shell.exec('git init', {cwd: dir, silent: true});
    if (res.code !== 0) {
      throw new Error(`git init exited with code ${res.code}.
stderr: ${res.stderr}
stdout: ${res.stdout}`);
    }
    // Doesn't matter currently
    shell.exec('git config user.email "test@jc-verse.com"', {
      cwd: dir,
      silent: true,
    });
    shell.exec('git config user.name "Test"', {cwd: dir, silent: true});

    shell.exec('git commit --allow-empty -m "First commit"', {
      cwd: dir,
      silent: true,
    });
  }
  commit(msg: string, date: string, author: string): void {
    const addRes = shell.exec('git add .', {cwd: this.dir, silent: true});
    if (addRes.code !== 0) {
      throw new Error(`git add exited with code ${addRes.code}.
stderr: ${addRes.stderr}
stdout: ${addRes.stdout}`);
    }
    }
  }
}

// This function is sync so the same mock repo can be shared across tests
export function createTempRepo(): {repoDir: string; git: Git} {
  const repoDir = fs.mkdtempSync(path.join(os.tmpdir(), 'git-test-repo'));

  const git = new Git(repoDir);

  return {repoDir, git};
}
