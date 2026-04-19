/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const fbPath = /\/(.*\/)?fb\//;

function stripFbImports() {
  return (tree) => {
    const stripped = new Set();
    const newChildren = [];

    for (const node of tree.children) {
      if (node.type !== 'mdxjsEsm' || !node.data?.estree?.body) {
        newChildren.push(node);
        continue;
      }

      const keptStatements = [];
      const removedSpecifiers = [];

      for (const stmt of node.data.estree.body) {
        if (
          stmt.type === 'ImportDeclaration' &&
          fbPath.test(stmt.source.value)
        ) {
          for (const sp of stmt.specifiers ?? [])
            removedSpecifiers.push(sp.local.name);
        } else {
          keptStatements.push(stmt);
        }
      }

      for (const name of removedSpecifiers) stripped.add(name);

      if (keptStatements.length === 0) continue;

      if (keptStatements.length === node.data.estree.body.length) {
        newChildren.push(node);
      } else {
        const keptLines = node.value
          .split('\n')
          .filter((line) => !fbPath.test(line));
        newChildren.push({
          ...node,
          value: keptLines.join('\n'),
          data: {
            ...node.data,
            estree: { ...node.data.estree, body: keptStatements },
          },
        });
      }
    }

    tree.children = newChildren;
    if (stripped.size > 0) stripJsx(tree, stripped);
  };
}

function stripJsx(node, names) {
  if (!node.children) return;
  node.children = node.children.filter((c) => {
    if (c.type.startsWith('mdxJsx') && names.has(c.name)) return false;
    stripJsx(c, names);
    return true;
  });
}

module.exports = stripFbImports;
