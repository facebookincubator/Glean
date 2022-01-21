/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

__attribute__((cpu_specific(ivybridge)))
void f1();

__attribute__((annotate("Hello"), annotate("Test")))
void f2();

#define CK_RENDER __attribute__((annotate("OnRender")))

CK_RENDER
void render();
