#!/usr/bin/env python3
# isort:skip_file

#
# Example Glean queries using Python.  Examples to try:
#
# Search for a function definition:
#  example.par --example 3 --param initFacebook
#
# Find cross-references in a file:
#  example.par --example 0 --param fbcode/common/init/Init.cpp
#
# Find function declarations in a file:
#  example.par --example 1 --param fbcode/common/init/Init.cpp
#

import argparse
import logging
import sys

import glean.schema.cxx1.ttypes  # noqa
import glean.schema.query.cxx1.ttypes as glean_query_cxx
import glean.schema.query.src.ttypes as glean_query_src
import glean.schema.src.ttypes  # noqa
from glean.client.client import GleanClient
from glean.client_config.ttypes import ClientConfig
from glean.glean.GleanService import UserQueryOptions
from glean.service.ttypes import HostPort


logger = logging.getLogger(__name__)


line = "-" * 75 + "\n"


def example0(glean, file):
    # -----------------------------------------------------------------
    # Example 0: cross-references for a source file

    # First find the file. This is done with a raw JSON query rather
    # than using the Thrift query types, just for illustration.
    facts = glean.jsonQuery(glean_query_src.File, '{ "key": "' + file + '" }')
    print(facts)

    # Find the XRefs for the file we found above.
    fileid = facts[0].id
    facts = glean.query(  # a query...
        glean_query_cxx.FileXRefs(  # for a FileXRefs predicate...
            key=glean_query_cxx.FileXRefs_key(  # with a key that matches..
                xmap=glean_query_cxx.FileXRefMap(
                    key=glean_query_cxx.FileXRefMap_key(
                        file=glean_query_src.File(id=fileid)  # this "file" field
                    )
                )
            )
        )
    )
    print(facts)

    # TODO: we might want to reverse the order of the args in the thrift
    # types, so that we can say src.File("foo")


def example1(glean, file):
    # -----------------------------------------------------------------
    # Example 1: find all function declarations in a file
    #
    # (Note: this arbitrarily picks the *first* trace for the given
    # file.  For a source file there will usually be just one trace,
    # but for a header file there might be multiple traces with
    # different sets of declarations, because the header file can be
    # included from different places with different preprocessor
    # defines in force, for example. To pick a particular trace for a
    # header file, start from a cxx.TranslationUnitTrace for a source
    # file, find the PPEvent for the include you're interested in, and
    # use the cxx.Trace inside.)

    facts = glean.query(
        glean_query_cxx.Trace(
            key=glean_query_cxx.Trace_key(file=glean_query_src.File(key=file))
        ),
        options=UserQueryOptions(max_results=1),
    )
    print(line, str(len(facts)) + " results, such as:")
    print(facts[0])

    # Fetch the declarations afterwards, to demonstrate glean.get():
    for trace in facts:
        glean.get(trace.key.declarations, recursive=True)

    print(facts[0])


def example2(glean, name):
    # -----------------------------------------------------------------
    # Example 2: Find the declaration(s) for a particular function name

    facts = glean.query(
        glean_query_cxx.FunctionDeclaration(
            key=glean_query_cxx.FunctionDeclaration_key(
                signature=glean_query_cxx.Signature(),  # unpack
                source=glean_query_src.Range(file=glean_query_src.File()),  # unpack
                name=glean_query_cxx.FunctionQName(
                    key=glean_query_cxx.FunctionQName_key(
                        name=glean_query_cxx.FunctionName(
                            key=glean_query_cxx.FunctionName_key(
                                name=glean_query_cxx.Name(key=name)
                            )
                        )
                    )
                ),
            )
        )
    )
    print(facts)


def example3(glean, name):
    # -----------------------------------------------------------------
    # Example 3: Like (2), but using recursive expansion

    facts = glean.queryRec(
        glean_query_cxx.FunctionDefinition(
            key=glean_query_cxx.FunctionDefinition_key(
                declaration=glean_query_cxx.FunctionDeclaration(
                    key=glean_query_cxx.FunctionDeclaration_key(
                        name=glean_query_cxx.FunctionQName(
                            key=glean_query_cxx.FunctionQName_key(
                                name=glean_query_cxx.FunctionName(
                                    key=glean_query_cxx.FunctionName_key(
                                        name=glean_query_cxx.Name(key=name)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    print(facts)


def main():
    parser = argparse.ArgumentParser(description="Python client for Glean")
    parser.add_argument("--tier", dest="tier", help="SMC tier for Glean.")
    parser.add_argument("--host", dest="host", help="Host that's running the service.")
    parser.add_argument("--port", dest="port", help="Port that's running the service.")
    parser.add_argument(
        "--repo-name",
        dest="repo_name",
        default="fbsource",
        help="Name of the repo to query (default: fbsource)",
    )
    parser.add_argument(
        "--repo-hash",
        dest="repo_hash",
        help="Hash of the repo to query (default: latest)",
    )
    parser.add_argument(
        "--example", dest="example", default="1", help="Selects example query"
    )
    parser.add_argument("--param", dest="param", default="", help="Parameter for query")

    options = parser.parse_args()

    config = None
    if options.tier is not None:
        config = ClientConfig()
        config.serv.set_tier(options.tier)
    elif options.host is not None:
        config = ClientConfig()
        config.serv.set_hostPort(HostPort(options.host, options.port))

    glean = GleanClient(
        repo_name=options.repo_name, repo_hash=options.repo_hash, config=config
    )

    if options.param == "":
        raise Exception("--param option missing")

    if options.example == "0":
        example0(glean, options.param)
    elif options.example == "1":
        example1(glean, options.param)
    elif options.example == "2":
        example2(glean, options.param)
    else:
        example3(glean, options.param)


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    sys.exit(main())
