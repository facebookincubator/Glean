/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

//@flow

function plus(x: number) { return x+1; };

module.exports.a = 1;
module.exports.b = true;
module.exports.c = 'three';
module.exports = plus;
