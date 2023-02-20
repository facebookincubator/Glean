#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

import argparse

from libfb.py import db_locator


def main():
    parser = argparse.ArgumentParser(description="Insert snapshot into database")
    parser.add_argument("--tier", required=True, help="the xdb tier")
    parser.add_argument("--repo", required=True, help="the repository name")
    parser.add_argument("--revision", required=True, help="the revision number")
    parser.add_argument("--file", required=True, help="the filename")
    parser.add_argument("--snapshot", help="snapshot data (for testing)")
    parser.add_argument("--snapshot-file", help="the snapshot data file path")
    args = parser.parse_args()

    locator = db_locator.Locator(tier_name=args.tier, role="scriptrw")
    locator.do_not_send_autocommit_query()
    conn = locator.create_connection()
    cursor = conn.cursor()

    if args.snapshot and args.snapshot_file:
        parser.error("You cannot specify both --snapshot and --snapshot-file")

    if args.snapshot:
        snapshot = args.snapshot
    elif args.snapshot_file:
        with open(args.snapshot_file, "rb") as f:
            snapshot = f.read()
    else:
        parser.error("You must specify either --snapshot or --snapshot-file")

    add_snapshot = (
        "INSERT INTO snapshot "
        "(repo, revision, file, snapshot) "
        "VALUES (%s, %s, %s, %s)"
    )

    cursor.execute(add_snapshot, (args.repo, args.revision, args.file, snapshot))
    print(cursor.fetchall())

    cursor.close()


if __name__ == "__main__":
    main()
