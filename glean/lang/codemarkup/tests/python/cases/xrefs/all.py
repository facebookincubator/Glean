# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

from big_lib import big_func, big_var
from lib import HelperClass as Helper

var = 123


def fn():
    """Another comment

    Here is a comment
    """
    pass


class LocalClass:
    pass


__all__ = ["Helper", "big_func", "LocalClass", "fn", "var", "Helper", "big_var"]
