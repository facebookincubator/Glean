# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from typing import TypeVar, Union

from big_lib import big_func as func  # noqa


class HelperClass:
    THelperClass = TypeVar("THelperClass", bound="HelperClass")
    value: THelperClass

    def method(self) -> "HelperClass":
        pass

    def another_method(self, param: "HelperClass") -> "Union[HelperClass, None]":
        pass


def helper_func():
    return HelperClass()
