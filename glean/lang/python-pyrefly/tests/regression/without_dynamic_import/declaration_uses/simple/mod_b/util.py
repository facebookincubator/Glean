# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa


def calc() -> None:
    pass


async def only_indirectly() -> None:
    calc()


class Cls:
    some_var = None

    def meth(self, arg):
        return calc()
