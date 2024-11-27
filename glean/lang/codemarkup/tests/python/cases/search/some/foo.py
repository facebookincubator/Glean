# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

# flake8: noqa
from .lib import a


class Foo:
    def meth(self, arg):
        a.some_func()


def foo():
    local_var = 1


foo_var = 1
