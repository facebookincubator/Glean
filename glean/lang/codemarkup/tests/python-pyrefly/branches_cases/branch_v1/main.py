# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# Branch V1 - Original implementation
from lib import helper_function, HelperClass
from utils import calculate_result


def main_function():
    """Main function for branch v1."""
    helper = HelperClass()
    result = helper.get_value()
    calculated = calculate_result(result)
    return calculated


class MainClass:
    """Main class for branch v1."""

    def __init__(self):
        self.helper = HelperClass()

    def process(self):
        """Process data using helper."""
        value = self.helper.get_value()
        return helper_function() + str(value)


# Global variable
BRANCH_VERSION = "v1"
