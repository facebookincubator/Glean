# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from big_lib import big_func as func  # noqa


class HelperClass:
    def method(self) -> "HelperClass":
        pass


def helper_func():
    return HelperClass()
