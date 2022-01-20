# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

# flake8: noqa

from big_lib import big_func as func  # noqa


class HelperClass:
    def method(self) -> "HelperClass":
        pass


def helper_func():
    return HelperClass()
