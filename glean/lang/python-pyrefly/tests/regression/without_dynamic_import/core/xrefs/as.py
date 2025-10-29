# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

from typing import Dict, Optional, Union

import all  # noqa
import big_lib as big
import lib
import main as unused  # noqa


def fn():
    """Test title

    extra comment body
    """
    local_var = lib.HelperClass(big.big_var)
    return local_var


@staticmethod
async def func_sleep():
    return 1


def test_fun(
    param0,
    /,
    param1: Optional[Dict[str]],
    param2: None,
    *,
    param3: Optional[str] = "value",
    **kwargs,
) -> Dict[str, Dict[str, bytes]]:
    return {}


def test_more_fun(
    param0: str = "value",
    *extrargs: Optional[str],
) -> Optional[bytes]:
    return {}


def pep570_posonly_test_kwonly(
    ok: Union[bytearray, str],
    hmm: Union[bool, str] = ("interesting"),
    /,
    what: Union[bytes, str] = ("umm getting scary"),
    *is_this_ok: Union[float, str],
    get_me_out_of_here: Union[int, str],
) -> bool:
    """
    Horrible test for type signature pretty printer
    """
    return False


def more_fun_things(
    a: int, /, *list_of_fish, something=None, **extra_bag_of_fun
) -> bool:
    return True
