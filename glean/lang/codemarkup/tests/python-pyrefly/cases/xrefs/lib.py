# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from abc import ABC, abstractmethod
from typing import ClassVar, final

from big_lib import big_func as func  # noqa


@final
class HelperClass:
    old_style_class_var = 1

    class_var: ClassVar[bool] = True
    instance_var: int

    def method(self) -> "HelperClass":
        pass

    @classmethod
    def class_method(cls) -> None:
        pass

    @staticmethod
    def static_method() -> None:
        pass

    @final
    @property
    def prop() -> int:
        pass

    def _protected_method() -> int:
        pass

    def __private_method() -> int:
        pass


def helper_func():
    return HelperClass()


class Parent1:
    pass


class Parent2:
    pass


class Child(Parent1, Parent2):
    pass


class AbstractClass(ABC):
    @abstractmethod
    def abstract_method(self): ...
