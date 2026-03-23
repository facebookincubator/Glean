# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from .lib import a


class Foo:
    def meth(self, arg):
        a.some_func()


def foo():
    local_var = 1


foo_var = 1


class In1:
    class In2:
        class In3:
            class In4:
                class In5:
                    class In6:
                        class In7:
                            class In8:
                                class In9:
                                    def meth_9(self):
                                        return none

                                def meth_8(self):
                                    return none

                            def meth_7(self):
                                return none

                        def meth_6(self):
                            return none

                    def meth_5(self):
                        return none

                def meth_4(self):
                    return none

            def meth_3(self):
                return none

        def meth_2(self):
            return none

    def meth_1(self):
        return none
