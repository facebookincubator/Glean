# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

"""Utility functions for branch v1."""


def calculate_result(value):
    """Calculate result for v1 - simple multiplication."""
    return value * 2


def format_output(data):
    """Format output for v1."""
    return f"v1_result: {data}"
