# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from mod_b import helper
from mod_b.helper import only_indirectly
from mod_b.util import Cls


def main() -> None:
    print("hello")
    only_indirectly()
    helper.Cls
    Cls.some_var


main()
