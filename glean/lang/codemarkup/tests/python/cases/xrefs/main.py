# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

# flake8: noqa

from big_lib import big_func  # noqa
from lib import HelperClass as Helper, func, helper_func


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
