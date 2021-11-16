// Copyright (c) Facebook, Inc. and its affiliates.

typedef int* test_init_type_t;

/** 
 *
 * @LionizerInit test_init_type_t 
 */
void f1(test_init_type_t test_init_var){
  test_init_var = nullptr;
};
