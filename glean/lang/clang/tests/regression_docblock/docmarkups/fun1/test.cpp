/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef int* test_init_type_t;

/** 
 *
 * @LionizerInit test_init_type_t 
 */
void f1(test_init_type_t test_init_var){
  test_init_var = nullptr;
};
