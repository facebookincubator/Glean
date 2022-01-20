/**
 * (c) Facebook, Inc. and its affiliates. Confidential and proprietary.
 */

//@flow

function plus(x: number) { return x+1; };

module.exports.a = 1;
module.exports.b = true;
module.exports.c = 'three';
module.exports = plus;
