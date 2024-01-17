# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# Integration test for the glass-snapshot binary
#   build glean db from test source
#   computes and uploads snaphots to xdb test shard
#   retrieve snaphots from xdb, deserialize and check number of refs/defs
#

import gzip
import shutil
import subprocess

import tempfile
from importlib import resources
from unittest import TestCase

from common.db.tests import DbDef
from glean.snapshot.thrift_types import Snapshot
from thrift.python.serializer import deserialize, Protocol


files = [
    "www/Attribute.php",
    "www/ClassA.php",
    "www/ClassB.php",
    "www/ClassC.php",
    "www/EnumA.php",
    "www/InterfaceA.php",
    "www/NS1.php",
    "www/NS1Sub1.php",
    "www/TopLevel.php",
    "www/TraitA.php",
    "www/modifiers.php",
]

db_name = "tests_cases_declarations/0"

revision = "c00c5b5e9edaaaccfa94fea2b5f9031ca28ee145"

expected = [
    ("Attribute.php", (1, 0)),
    ("ClassA.php", (4, 1)),
    ("ClassB.php", (7, 3)),
    ("ClassC.php", (8, 16)),
    ("InterfaceA.php", (2, 0)),
    ("NS1.php", (4, 0)),
    ("NS1Sub1.php", (2, 0)),
    ("TopLevel.php", (19, 13)),
    ("TraitA.php", (3, 3)),
    ("modifiers.php", (4, 0)),
]

# try to deserialize and extract number of defs and refs
def _extract_defs_refs(snapshot):
    decompressed_data = gzip.decompress(snapshot)
    snapshot = deserialize(
        Snapshot,
        decompressed_data,
        protocol=Protocol.COMPACT,
    )
    res = snapshot.queries[0].result
    defs = len(res.definitions)
    refs = len(res.references)
    return (defs, refs)


class SnapshotIntegrationTest(TestCase):
    def test_glass_snapshot(self):
        test_dir = tempfile.TemporaryDirectory().name

        # extract package path to get the test files specified as a buck resource
        pkg = ".".join(__package__.split(".")[0:-1])
        resource_dir = (
            str(resources.path(pkg, "test_files")) + "/snapshots/test-data/declarations"
        )

        # this is a bit of a hack. The indexer doesn't like symlinks and very long path
        # copying the resource files to a temp dir solve both issues.
        shutil.copytree(resource_dir, test_dir, symlinks=False, ignore=None)

        db_root = tempfile.TemporaryDirectory().name
        index_command = [
            "buck2",
            "--isolation-dir",
            "snapshot-test",
            "run",
            "//glean/tools/gleancli:glean",  # @dep=//glean/tools/gleancli:glean
            "--",
            "--db-root",
            db_root,
            "index",
            "hack",
            "--db",
            db_name,
            test_dir,
        ]
        print("building db: ", " ".join(index_command))
        subprocess.run(index_command)

        test_db_manager = DbDef.TestDatabaseManager(
            oncall_shortname="code_indexing", source_shard="xdb.glass_snapshot_dev"
        )

        print("creating test shard")
        tier = test_db_manager.get_shard_name()

        command = [
            "buck2",
            "--isolation-dir",
            "snapshot-test",
            "run",
            "//glean/glass:glass-snapshot",  # @dep=//glean/glass:glass-snapshot
            "--",
            "--revision",
            "revision",
            "--db-root",
            db_root,
            "--db",
            db_name,
            "--snapshot-tier",
            tier,
        ] + files
        print("building snapshot: ", " ".join(command))
        subprocess.run(command)
        print("querying xdb")
        conn = test_db_manager.get_connection()
        rows = conn.query("SELECT file, snapshot FROM snapshot")
        actual = sorted(
            [(row["file"], _extract_defs_refs(row["snapshot"])) for row in rows]
        )

        self.assertEqual(actual, expected, "computed snapshot doesn't match expected")
