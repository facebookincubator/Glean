# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

"""Helper library for branch v1."""


class HelperClass:
    """A helper class for branch v1."""

    def __init__(self):
        self.value = 42
        self.name = "helper_v1"

    def get_value(self):
        """Return the stored value."""
        return self.value

    def set_value(self, new_value):
        """Set a new value."""
        self.value = new_value


def helper_function():
    """A helper function for v1."""
    return "helper_v1"
