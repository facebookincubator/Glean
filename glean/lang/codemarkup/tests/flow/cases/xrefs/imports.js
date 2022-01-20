/**
 * (c) Facebook, Inc. and its affiliates. Confidential and proprietary.
 */

//@flow

const {a, b, c} = require('./cjs_exports');

import d, {foo, bar, baz} from './es_exports';
import * as qux from './es_exports';

import fn from 'cjs_exports';  // ES import -> commonJS export
  // also use a haste module name

import type {num} from './es_exports';

let e: string = d;

let x: num = bar(a);
let y: num = qux.bar(foo);
let z: num = fn(3);

class caz extends baz {}

import typeof str, {C} from './es_exports';

type D = C;
let s: str = "abc";

// ToDo:
//  - entity for refs is wrong: points to import not export
//  - other combinations of commonJS/ES?
