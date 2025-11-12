# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

from typing import Optional, Union

import bigger_module.submodule
import some_module

from bar import Bar
from bigger_module.another_submodule import SubmoduleClass
from generic import Generic
from optional_generic import OptionalGeneric


# pyre-ignore [13]
class Foo:
    name: str
    year: int
    my_bar: Bar
    optional_bar: Optional[Bar]


def return_optional_generic() -> Optional[OptionalGeneric[int]]:
    return OptionalGeneric[int]()


def baz(foo: Foo) -> None:
    # foo.name correctly resolves to Foo.name
    # foo.name.capitalize() correctly resolves to str.capitalize()
    local_str = foo.name.capitalize()
    # local_str's key is not resolved to str. We want to preserve local references
    placeholder_local_str = local_str
    # local_str's type information doesn't cause placeholder_local_str's key to resolve to str from previous line
    placeholder_local_str.capitalize()
    # attribute references to class/module's functions are still resolving correctly
    local_str.capitalize()
    # nested attributes are correctly resolved.
    foo.my_bar.bar_attribute
    # Optional[Class] should try its best to reference Class's attributes
    foo.optional_bar.bar_method()
    # Class[AnotherClass] should try to reference Class's attributes
    test_generic: Generic[int] = Generic[int]()
    test_generic.test_method()
    # Optional[Class[int]] should try to reference Class's attributes
    optional_generic: Optional[OptionalGeneric[int]] = return_optional_generic()
    optional_generic.test_method()


# Test FunctionDef returns
def return_type_not_set():
    return


def return_type_none() -> None:
    return None


def return_type_class() -> Bar:
    return Bar()


def return_type_submodule_class() -> SubmoduleClass:
    return SubmoduleClass()


def return_type_class_generic() -> Generic[int]:
    return Generic[int]()


def return_type_nested_generic() -> Generic[Generic[int]]:
    return Generic[Generic[int]]()


def return_union_type() -> Union[Bar, Generic]:
    return Bar()


def return_fully_qualified_types() -> some_module.SomeClass:
    return some_module.SomeClass()


def return_pyre_infered_type() -> SubmoduleClass.InnerClass:
    return SubmoduleClass.InnerClass()


def return_submodule_types() -> bigger_module.submodule.SomeClass:
    return bigger_module.submodule.SomeClass()


def return_optional_union_generic() -> Optional[Union[Generic[int], Generic[Foo]]]:
    return None


# Test inner functions
def function() -> SubmoduleClass:
    def inner_function() -> SubmoduleClass:
        return SubmoduleClass()

    return inner_function()


foo = Foo()
# foo.name correctly resolves to Foo.name
# foo.name.capitalize() correctly resolves to str.capitalize()
global_str = foo.name.capitalize()
# glocal_str's key is not resolved to str. We want to preserve reference
placeholder_glocal_str = global_str
# _glocal_str's type information doesn't cause placeholder_glocal_str's key to resolve to str from previous line
placeholder_glocal_str.capitalize()
# attribute references to class/module's functions are still resolving correctly
global_str.capitalize()
# Foo().name correctly resolves to Foo.name
# Foo().name.capitalize() correctly resolves to str.capitalize()
# Foo().name.capitalize().lower() correctly resolves to str.lower()
Foo().name.capitalize().lower()
