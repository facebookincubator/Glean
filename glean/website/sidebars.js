/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {fbContent, fbInternalOnly} = require('internaldocs-fb-helpers');

module.exports = {
  someSidebar: {
       'Quick Start': [
           'introduction',
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
                   ...fbContent({
                       internal: ['schema/fb/workflow'],
                       external: ['schema/workflow'],
                   }),
                   'schema/thrift',
               ]
           },
           {
               'Angle': [
                   'angle/intro',
                   'angle/guide',
                   'angle/efficiency',
                   'angle/advanced',
                   'angle/debugging',
                   'angle/reference',
               ],
           },
           'derived',
           'databases',
           'write',
           'shell',
           'server',
           'cli',
       ],
  },
};
