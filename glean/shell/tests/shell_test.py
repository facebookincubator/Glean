# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import, division, print_function, unicode_literals

import json
import re
import sys

import pexpect
from libfb.py.pathutils import find_path_in_fbcode_bin, find_path_in_fbcode_dir
from libfb.py.testutil import BaseFacebookTestCase


GLEAN_PATH = find_path_in_fbcode_bin("glean/tools/gleancli/glean")
MKTESTDB_PATH = find_path_in_fbcode_bin("glean/test/lib/make_test_db")
SCHEMA_PATH = find_path_in_fbcode_dir("glean/schema/source")
EXAMPLE_SCHEMA_PATH = find_path_in_fbcode_dir("glean/example/schema")
EXAMPLE_FACTS_PATH = find_path_in_fbcode_dir("glean/example/facts.glean")

REPO = "dbtest-repo"
DB = REPO + "/f00baa"
PROMPT = REPO + ">"


class GleanShellTest(BaseFacebookTestCase):
    tmpdir = None
    process = None

    @classmethod
    def startShell(cls, db, schema=SCHEMA_PATH):
        cls.tmpdir = pexpect.run("mktemp -d", encoding="utf8").strip()

        pexpect.run("mkdir " + cls.tmpdir + "/db")
        pexpect.run("mkdir " + cls.tmpdir + "/schema")
        pexpect.run(MKTESTDB_PATH + " " + cls.tmpdir + "/db")
        pexpect.run("ls " + cls.tmpdir + "/db")

        if db is None:
            db_args = []
        else:
            db_args = ["--db=" + db if db != "" else ""]

        if schema is None:
            schema_args = ["--schema=" + cls.tmpdir + "/schema"]
        else:
            schema_args = ["--schema=" + schema]

        cls.process = pexpect.spawn(
            GLEAN_PATH,
            logfile=sys.stdout,
            encoding="utf8",
            args=["--db-root=" + cls.tmpdir + "/db"]
            + schema_args
            + ["shell"]
            + db_args,
        )

        if db is None:
            cls.process.expect(">")
        else:
            cls.shellCommand(":database " + db)
            cls.process.expect(PROMPT)

    @classmethod
    def setUp(cls):
        cls.startShell(DB)

    @classmethod
    def tearDown(cls):
        if cls.process:
            cls.process.sendline(":quit")
            cls.process.expect(pexpect.EOF)

    @classmethod
    def __del__(cls):
        if cls.tmpdir:
            pexpect.run("rm -rf " + cls.tmpdir)

    @classmethod
    def shellCommand(cls, cmd, prompt=PROMPT):
        cls.process.sendline(cmd)
        cls.process.expect(prompt)
        return cls.process.before


class GleanShellReload(GleanShellTest):
    @classmethod
    def setUp(cls):
        cls.startShell(None, None)

    def test(self):
        pexpect.run(
            "cp " + EXAMPLE_SCHEMA_PATH + "/example.angle " + self.tmpdir + "/schema"
        )
        self.shellCommand(":reload", ">")
        self.shellCommand(":load " + EXAMPLE_FACTS_PATH, "facts>")

        # add a new derived predicate to the schema
        with open(self.tmpdir + "/schema/example.angle", "a") as f:
            f.write(
                "schema example.2 : example.1 {"
                + "  predicate Foo:string S where Class {S,_ }"
                + "}"
                + "schema all.2 : example.2 {}"
            )
        self.shellCommand(":reload", "facts")

        # check that we can query for the new derived predicate
        output = self.shellCommand("example.Foo.2 _", "facts>")
        self.assertIn("Fish", output)


class GleanShellNoDB(GleanShellTest):
    @classmethod
    def setUp(cls):
        cls.startShell(None)

    def test(self):
        output = self.shellCommand(":database " + DB)
        self.assertIn(DB, output)


class GleanShellListDBs(GleanShellTest):
    def test(self):
        output = self.shellCommand(":list")
        self.assertIn(DB, output)

        # With filter argument
        output = self.shellCommand(":list dbtest-repo")
        self.assertIn(DB, output)

        # With full DB argument
        output = self.shellCommand(":list " + DB)
        self.assertIn(DB, output)

        # With non-existent repo filter argument
        output = self.shellCommand(":list fakerepo")
        self.assertNotIn(DB, output)


class GleanShellStatistics(GleanShellTest):
    def test(self):
        output = self.shellCommand(":statistics")
        self.assertIsNotNone(re.search("glean.test.Predicate.1\r\n *count: 2", output))
        self.assertIsNotNone(re.search("sys.Blob.1\r\n *count: 2", output))
        self.assertIsNotNone(
            re.search("Total: \\d+ facts \\(\\d+\\.\\d+ kB\\)", output)
        )


class GleanShellPredicates(GleanShellTest):
    def test(self):
        output = self.shellCommand(":schema")
        self.assertIsNotNone(re.search("glean.test.Predicate", output))


class GleanShellQuery(GleanShellTest):
    def test(self):
        self.shellCommand(":mode json")

        # A query with no pattern - match all the facts.
        output = self.shellCommand("sys.Blob")
        self.assertIn('"hello"', output)
        self.assertIn('"bye"', output)

        # A query with a pattern
        output = self.shellCommand(
            'glean.test.Predicate.1 { "named_sum_" : { "tue" : 37 } }'
        )
        self.assertIn("1 results", output)
        self.assertIn('"byt": 33', output)

        # Match and recursively expand
        output = self.shellCommand(
            'glean.test.Predicate.4 { "pred" : { "key" : "bye" } }'
        )
        self.assertIn("2 results, 6 facts", output)

        # Recursively expand a fact by Id
        self.shellCommand(":limit 1")
        output = self.shellCommand("cxx1.FunctionName ")
        fact1 = output[output.find("{ ") : output.rfind("}") + 1]
        j = json.loads(fact1)
        output = self.shellCommand("{" + str(j["id"]) + "}")
        self.assertIn("2 facts", output)
        fact2 = output[output.find("{ ") : output.rfind("}") + 1]
        self.assertEqual(fact1, fact2)


class GleanShellLoad(GleanShellTest):
    def test(self):
        repo = "test"
        prompt = repo + ">"
        self.shellCommand(
            ":load " + repo + "/0 glean/shell/tests/expr.glean", prompt=prompt
        )
        output = self.shellCommand(":db", prompt=prompt)
        self.assertIn("test/0", output)

        output = self.shellCommand(":stat", prompt=prompt)
        self.assertIsNotNone(re.search("glean.test.Expr.1\r\n *count: 6", output))

        output = self.shellCommand(
            'glean.test.Expr { lam = { var_ = "x" } }', prompt=prompt
        )
        self.assertIn("1 results", output)

        # :load <file> should choose the repo name automatically:
        self.shellCommand(":load glean/shell/tests/expr.glean", prompt="expr>")
        output = self.shellCommand(":db", prompt="expr>")
        self.assertIn("expr/0", output)

        # :load <file> again should choose a new unique repo name:
        self.shellCommand(":load glean/shell/tests/expr.glean", prompt="expr>")
        output = self.shellCommand(":db", prompt="expr>")
        self.assertIn("expr/1", output)


class GleanShellOwner(GleanShellTest):
    def test(self):
        repo = "owner"
        prompt = repo + ">"

        # facts in owner.glean replicate the test setup from IncrementalTest.hs
        self.shellCommand(
            ":load " + repo + "/0 glean/shell/tests/owner.glean", prompt=prompt
        )
        output = self.shellCommand(":db", prompt=prompt)
        self.assertIn("owner/0", output)

        # 1024 should be the first fact created, i.e. Node "d"
        output = self.shellCommand(":!owner 1024", prompt="owner>")
        self.assertIn("D || B || C", output)


class GleanShellDump(GleanShellTest):
    def test(self):
        dumpfile = self.tmpdir + "/test.glean"
        self.shellCommand(":dump " + dumpfile)
        repo = "test"
        prompt = repo + ">"
        self.shellCommand(":load " + repo + "/0 " + dumpfile, prompt=prompt)
        output = self.shellCommand(":stat", prompt=prompt)
        self.assertIsNotNone(re.search("sys.Blob.1\r\n *count: 2", output))


class GleanShellCompletion(GleanShellTest):
    def test(self):
        # test completing the argument of :schema
        output = self.shellCommand(":schema glean.test.Ex\t")
        self.assertIsNotNone(re.search("predicate glean.test.Expr.1 :", output))
        # test completing a predicate name in a query
        output = self.shellCommand('gl\tP\t { string_ = "abba" }')
        # test that we completed to the correct thing
        self.assertIn("1 results", output)


class GleanShellAngle(GleanShellTest):
    def test(self):
        self.shellCommand(":mode angle")

        # A query with no pattern - match all the facts.
        output = self.shellCommand("sys.Blob _")
        self.assertIn('"hello"', output)
        self.assertIn('"bye"', output)

        # A query with a pattern
        output = self.shellCommand(
            "glean.test.Predicate.1 { named_sum_ = { tue = 37 }}"
        )
        self.assertIn("1 results", output)
        self.assertIn('"byt": 33', output)

        # Match and recursively expand
        output = self.shellCommand(
            'B = sys.Blob "bye"; glean.test.Predicate.4 { pred = B }'
        )
        self.assertIn("2 results", output)

        # Recursively expand a fact by Id
        self.shellCommand(":limit 1")
        output = self.shellCommand("cxx1.FunctionName _")
        fact1 = output[output.find("{ ") : output.rfind("}") + 1]
        j = json.loads(fact1)
        output = self.shellCommand("{" + str(j["id"]) + "}")
        self.assertIn("2 facts", output)
        fact2 = output[output.find("{ ") : output.rfind("}") + 1]
        self.assertEqual(fact1, fact2)

        # Test querying for things that aren't full facts
        output = self.shellCommand('prim.toLower "AbC"')
        self.assertIn("abc", output)

        output = self.shellCommand("prim.length [0, 0, 0, 0]")
        self.assertIn('"key": 4', output)

        output = self.shellCommand('prim.length ["Hello", "World"]')
        self.assertIn('"key": 2', output)


class GleanShellQueryProfiling(GleanShellTest):
    def test(self):
        self.shellCommand(":profile full")

        output = self.shellCommand(
            'glean.test.Predicate { pred = "hello" } | '
            + "glean.test.Predicate { nat = 42 }"
        )
        self.assertIn("glean.test.Predicate.4 : 8", output)
        self.assertIn("sys.Blob.1 : 1", output)


class GleanShellQueryDebug(GleanShellTest):
    def test(self):
        self.shellCommand(":debug all")

        output = self.shellCommand("3")
        self.assertIn("ir:", output)
        self.assertIn("bytecode:", output)
