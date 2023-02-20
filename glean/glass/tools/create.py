#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

import argparse

from libfb.py import db_locator

# One can create an ondemand xdb tier using
#   ondemand devdb new
#   other useful commands: list, release, connect
# See  https://www.internalfb.com/intern/wiki/OnDemand/Development_Database/


def main():
    parser = argparse.ArgumentParser(description="Create snapshot table")
    parser.add_argument("--tier", required=True, help="the xdb tier")
    parser.add_argument(
        "--drop-if-exists", action="store_true", help="drop the table if it exists"
    )
    args = parser.parse_args()

    locator = db_locator.Locator(tier_name=args.tier, role="scriptrw")
    locator.do_not_send_autocommit_query()
    conn = locator.create_connection()
    cursor = conn.cursor()

    table = "snapshot"

    # TODO store query type + options
    create = (
        f"CREATE TABLE {table} (repo VARCHAR(255) NOT NULL,"
        "revision VARCHAR(255) NOT NULL,"
        "file VARCHAR(255) NOT NULL,"
        "snapshot MEDIUMBLOB NOT NULL)"  # 16MB
    )

    if args.drop_if_exists:
        create = f"DROP TABLE IF EXISTS {table};" + create

    cursor.execute(create, [])
    cursor.close()


if __name__ == "__main__":
    main()
