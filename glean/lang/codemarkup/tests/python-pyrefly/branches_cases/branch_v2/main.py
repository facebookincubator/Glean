# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# Branch V2 - Enhanced implementation
from lib import helper_function, HelperClass, new_helper_function
from utils import advanced_calculation, calculate_result


def main_function():
    """Main function for branch v2 - enhanced version."""
    helper = HelperClass()
    result = helper.get_value()
    calculated = calculate_result(result)
    advanced = advanced_calculation(calculated)
    return advanced


def new_main_function():
    """New function added in v2."""
    return new_helper_function()


class MainClass:
    """Main class for branch v2 - enhanced."""

    def __init__(self):
        self.helper = HelperClass()
        self.version = "v2"

    def process(self):
        """Process data using helper - enhanced version."""
        value = self.helper.get_value()
        enhanced_value = self.helper.get_enhanced_value()
        return helper_function() + str(value) + str(enhanced_value)

    def new_process(self):
        """New processing method in v2."""
        return new_helper_function()


# Global variable
BRANCH_VERSION = "v2"
