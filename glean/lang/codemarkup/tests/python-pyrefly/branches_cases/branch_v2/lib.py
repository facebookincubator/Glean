# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

"""Helper library for branch v2 - enhanced version."""


class HelperClass:
    """A helper class for branch v2 - enhanced."""

    def __init__(self):
        self.value = 42
        self.name = "helper_v2"
        self.enhanced_value = 100  # New in v2

    def get_value(self):
        """Return the stored value."""
        return self.value

    def get_enhanced_value(self):
        """Return the enhanced value - new in v2."""
        return self.enhanced_value

    def set_value(self, new_value):
        """Set a new value."""
        self.value = new_value

    def set_enhanced_value(self, new_value):
        """Set enhanced value - new in v2."""
        self.enhanced_value = new_value


def helper_function():
    """A helper function for v2."""
    return "helper_v2"


def new_helper_function():
    """New helper function added in v2."""
    return "new_helper_v2"
