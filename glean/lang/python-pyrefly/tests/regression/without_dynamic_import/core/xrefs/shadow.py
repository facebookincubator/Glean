# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from big_lib import big_func, big_var

local_var = 123


class ShadowClass:
    big_func = big_func
    other_var = big_var
    local_var = local_var


def shadow_func():
    big_func = shadow_func
    big_var = 1
    local_var = 2
    other_func = big_func
    other_var = big_var
    other_local_var = local_var


big_func = big_func
other_var = big_var
local_var = local_var
