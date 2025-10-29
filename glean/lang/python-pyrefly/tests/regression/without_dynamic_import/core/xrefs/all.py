# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

import timeit
from functools import lru_cache
from typing import List

import lib
from big_lib import big_func, big_var
from lib import HelperClass as Helper

var = 123

var1: List[int] = [1, 2, 3]

var2: List[int] = var1


@timeit
@lru_cache
def fn():
    local_var = lib.HelperClass(big_var)
    return local_var


class LocalClass:
    def meth(self, param) -> "Helper":
        return fn()


__all__ = ["Helper", "big_func", "LocalClass", "fn", "var", "Helper", "big_var"]
