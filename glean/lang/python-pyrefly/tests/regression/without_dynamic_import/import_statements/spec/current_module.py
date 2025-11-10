# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# from spec in T90159182
import u.v as w  # noqa F401
import x.y
from a import b, c as d

b(d)
x.f()
