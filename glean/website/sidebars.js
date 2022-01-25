/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {fbContent, fbInternalOnly} = require('internaldocs-fb-helpers');

module.exports = {
  someSidebar: {
       'Quick Start': [
           'introduction',
           'trying',
           'building',
           'walkthrough',
       ],
       'User Guide': [
           {
               'Schemas': [
                   'schema/basic',
                   'schema/types',
                   'schema/syntax',
                   'schema/recursion',
                   'schema/changing',
                   'schema/migration',
                   ...fbContent({
                       internal: ['schema/fb/workflow'],
                       external: ['schema/workflow'],
                   }),
                   'schema/thrift',
               ]
           },
           {
               'Querying': [
                   'query/intro',
                   {
                       'Angle': [
                           'angle/intro',
                           'angle/guide',
                           'angle/efficiency',
                           'angle/advanced',
                           'angle/debugging',
                           'angle/reference',
                           ...fbInternalOnly(['angle/fb/examples']),
                           'angle/style',
                       ],
                   },
                   {
                       'APIs': [
                           'query/api/haskell',
                           ...fbInternalOnly(['query/api/fb/hack']),
                           ...fbInternalOnly(['query/api/fb/python']),
                           ...fbInternalOnly(['query/api/fb/rust']),
                           ...fbInternalOnly(['query/api/fb/cpp']),
                       ],
                   },
                   ...fbInternalOnly(['query/fb/thrift']),
               ],
           },
           'derived',
           'databases',
           'write',
           'running',
           'shell',
           'server',
           'cli',
           {
               'Indexers': [
                   'indexer/intro',
                   'indexer/flow',
                   'indexer/hack',
               ],
           },
       ],
  },
};
