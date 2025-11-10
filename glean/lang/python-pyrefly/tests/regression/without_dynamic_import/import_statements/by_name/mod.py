# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa


# importing two things with the same alias should work
import os as p
import os.path.join
import string as p

from m import a, b as not_b_but_something_else
