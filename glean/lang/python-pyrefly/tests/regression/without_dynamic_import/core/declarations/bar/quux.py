# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.


from dataclasses import dataclass

value = 1
multiline_value = (True,)
x, y = 1, 2
foo = pow(1, 2)

comp = [x for x in range(start=1, stop=5)]  # noqa C416
expr = 1 + 5
l = lambda a, b, c: a + b + c + 10  # noqa E741


@dataclass(frozen=True)
class Foo:
    @property
    def func(self) -> None:
        try:
            pass
        except Exception as ex:  # noqa F841
            pass


with something() as w:  # noqa F841
    pass

for k in range(1, 5):
    print(k)
    print(k)

print("hello world")
print("helloworld")
print("hello", "world")

print("world", hello="hello")
print(b"world")

for var in range(10):
    print(var)
