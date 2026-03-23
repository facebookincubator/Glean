# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

from big_lib._inner import BaseClass

big_var = 0


def big_func():
    """A summary

    A comment goes here.
    A comment goes there.
    A comment over here too.
    """
    return big_var


@staticmethod
async def func_sleep():
    return 1


__all__ = ["big_func", "big_var", "func_sleep", "BaseClass"]
