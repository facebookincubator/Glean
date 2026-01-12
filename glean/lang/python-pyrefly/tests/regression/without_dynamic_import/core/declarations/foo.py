# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.


# flake8: noqa

#!/usr/bin/env python3
"""
Module Docstring
"""

import abc
from abc import ABC, abstractmethod
from typing import Annotated, Callable, final, Optional, Union

import a.b.c  # noqa
import x.y
from bar import Bar, µ
from glean.lang.python.indexers.core import path_for_pyre
from lib import helper_func, nonexistent_func  # noqa


try:
    from something import maybe_import
except ImportError:

    def maybe_import() -> None:
        """
        Global Scope Function Docstring
        """
        pass


class FooClass:
    """
    Class Docstring
    """

    foo_class_var: ClassVar[bool] = True
    foo_instance_var: int

    def method(self) -> None:
        """
        Function Scope Function Docstring
        """
        method_inner_variable = None
        """
        Local Variable Docstring
        """

        def method_inner_function() -> None:
            pass

    class FooInnerClass:
        def inner_method(self) -> None:
            def inner_method_inner_function() -> None:
                pass


def foo_func(param: int) -> bool:
    def inner_foo_func() -> None:
        pass

    class HiddenClass:
        def hidden_method(self):
            def hidden_method_inner_function() -> None:
                pass

    return True


sorted([[]], key=lambda x: len(x))


def pow() -> int:
    return 0


foo_global = False
"""
Global Variable Docstring
"""

new_var = 10
new_var += 5


@final
class FinalClass:
    @classmethod
    def class_method(cls) -> None:
        pass


class FooChildClass(FooClass):
    pass


class FooBar(Bar):
    pass


# Test FunctionDefinition
def param_types_test(
    self,
    builtin: int,
    basic: FooClass,
    defaulted: str = "abc" + "def",
    nested: Optional[FooClass] = None,
    union: Union[Optional[FooClass], Bar, Generic[int]] = None,
):
    pass


def param_keyword_test(
    builtin: int,
    key: Optional[FooClass] = None,
    *args,
    keyword_only: Optional[FooClass],
    **kwargs,
):
    pass


def param_positional_keyword_only_test(
    builtin: int,
    /,
    pos: int,
    key: Optional[FooClass] = None,
    *,
    keyword_only: Optional[FooClass],
    **kwargs,
):
    pass


def simple_return() -> FooClass:
    return FooClass()


def none_return():
    return


def none_ellipsis() -> ...:
    return


def string_literal_return() -> "FooClass":
    return FooClass()


def optional_return() -> Optional[FooClass]:
    return None


def union_return() -> Union[FooClass, FooBar]:
    return FooBar()


def pipe_union_return() -> Union[FooClass | FooBar]:
    return FooBar()


def callable_return() -> Callable[[str, Optional[bool]], Optional[int]]:
    return lambda x: None


def pep593_return() -> Annotated[
    int,
    ValueRange(3, [10, *star]),
    ctype("char"),
    dtype(kv={"k1": "v1", "k2": "v2", **star}),
]:
    return 3


def await_return() -> await Optional[int]:
    return None


def complex_return() -> Optional[
    Union[FooClass, Callable[[int], bool], int | str | None]
]:
    return None


def imported_inner_class_return() -> Bar.InnerBar:
    return Bar.InnerBar()


class AbstractFoo(ABC):
    @abstractmethod
    def abstract_method(self): ...

    @abc.abstractmethod
    def abc_abstract_method(self): ...


class Level0A:
    def method1(self) -> None:
        pass

    def method2(self) -> None:
        pass


class Level0B:
    def method1(self) -> None:
        pass


class Level1(Level0A, Level0B):
    def method1(self) -> None:
        pass

    def method2(self) -> None:
        pass


class Level2(Level1):
    def method2(self) -> None:
        pass
