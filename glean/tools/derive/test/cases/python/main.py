# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from big_lib import big_func  # noqa
from lib import func, helper_func, HelperClass as Helper


def f() -> "None":
    pass


def f() -> "None":
    helper_func()
    big_func()
    func()
    c = Helper()
    c.method()
    return c


def g() -> None:
    from lib import HelperClass as OtherHelper

    def h(_: OtherHelper) -> None:
        pass


if __name__ == "__main__":
    f()
