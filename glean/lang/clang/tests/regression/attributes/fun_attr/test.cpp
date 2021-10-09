// Copyright (c) Facebook, Inc. and its affiliates.

__attribute__((cpu_specific(ivybridge)))
void f1();

__attribute__((annotate("Hello"), annotate("Test")))
void f2();

#define CK_RENDER __attribute__((annotate("OnRender")))

CK_RENDER
void render();
