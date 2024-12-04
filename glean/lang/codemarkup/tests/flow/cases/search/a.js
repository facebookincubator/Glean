/**
 * (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.
 */

// @flow

/**
 * this comment documents the import of
 * foo and bar
 */
import { foo, bar } from './exports';

// this is a comment on a
let a: number = 1;
/** this is a comment on b */
var b: number = 2;
/* this is a comment on c */
const c: number = 3;

/* this is a comment on d */
function d(e : number) : number {
  return a + b + c + e;
}

/** this is a comment on f */
class f extends foo {
  prop2: number;
  g(): number {
    return this.prop1 + this.prop2 + b;
  }
}

// this is a comment on h and i
const {h, i} = { h: 1, i: 2 };

/* this is a comment on an enum */
enum MyEnum {
  Foo,
  Bar,
};

export { a, b, c, d, f, h, i }
