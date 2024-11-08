# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

"""
To quickly test yaml indexer on a yaml file:
    cd ~/fbsource/fbcode
    buck run glean/lang/yaml/indexers:yaml_indexer -- --output-dir temp/glean/yikai --file glean/lang/yaml/indexers/tests/test_1.yaml -p

To run the indexer on multiple yaml files, generate glean facts in a JSON file and store it in test glean DB:
    cd ~/fbsource/fbcode
    INPUT_CSV=glean/lang/yaml/indexers/tests/test_yaml_files.csv USER=yikai glean/lang/yaml/make_test_db.sh

If changes are made to the schema, you also need to run Glean from source:
    cd ~/fbsource/fbcode
    glean/scripts/run-local-server --db-root ~/glean-dbs
    GLEAN_TIER=localhost:25052 INPUT_CSV=glean/lang/yaml/indexers/tests/test_yaml_files.csv USER=yikai glean/lang/yaml/make_test_db.sh
"""

import csv
import json
import logging
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Union

import click
from glean.client.py3 import _glean_name, _schema_version
from glean.lang.python import facts
from glean.schema.python.types import Name
from glean.schema.src.types import ByteSpan, File
from glean.schema.yaml.types import XRefsByFile, XRefsByFile_key, XRefViaName
from phabricator.new_phabricator_graphql_helpers import PhabricatorPaste
from phabricator.phabricator_auth_strategy_factory import PhabricatorAuthStrategyFactory
from simplejson import JSONEncoder
from simplejson.raw_json import RawJSON
from thrift.py3 import Protocol, serialize

logger: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class IndexingResult:
    input: str
    facts: XRefsByFile


@dataclass
class FactsBatch:
    predicate: str
    facts: List[RawJSON]

    def for_json(self) -> Dict[str, Union[List[RawJSON], str]]:
        data = {
            "predicate": self.predicate,
            "facts": self.facts,
        }
        return data


@click.command()
@click.option(
    "--collect-sources-from-csvfile",
    type=click.Path(exists=True, resolve_path=True),
    help=("Read IndexableFiles from the specified csv file."),
)
@click.option(
    "--output-dir",
    type=click.Path(resolve_path=True),
    required=True,
    help=("Path to export directory (for json files)"),
)
@click.option(
    "--file",
    type=click.Path(exists=True, resolve_path=True),
    help=("Provide a yaml file path(relative to /fbcode) to run yaml indexer on."),
)
@click.option(
    "-p",
    "--create-paste",
    default=False,
    is_flag=True,
    help="create a paste with the JSON output",
)
def main(
    collect_sources_from_csvfile: Optional[str],
    output_dir: str,
    file: Optional[str],
    create_paste: bool,
) -> None:
    all_yaml_files = get_all_yaml_files(collect_sources_from_csvfile, file)
    all_indexing_results = [index_one(file) for file in all_yaml_files]
    all_facts_batch = [
        make_facts_batch(index_result) for index_result in all_indexing_results
    ]
    encoder: JSONEncoder = JSONEncoder(
        separators=(",", ":"), for_json=True, iterable_as_array=True
    )
    dirname = Path(output_dir)
    dirname.mkdir(parents=True, exist_ok=True)
    with (dirname / "0.json").open("w") as f:
        for json_chunk in encoder.iterencode(all_facts_batch, False):
            f.write(json_chunk)
            # TODO(T207069122) Log when yaml indexer's output exceeds size limit
    if create_paste:
        with (dirname / "0.json").open("r") as f:
            json_string = f.read()
            data = json.loads(json_string)
            formatted_json = json.dumps(data, indent=4)
            paste_client = PhabricatorPaste(
                PhabricatorAuthStrategyFactory.paste_bot(),
                "yaml_indexer_debug",
            )
            paste = paste_client.create_temp_everpaste(formatted_json)
            print("Generated JSON:")
            print(paste)

    logger.info(f"Finished writing to {dirname}/0.json")


def get_all_yaml_files(
    collect_sources_from_csvfile: Optional[str], file: Optional[str]
) -> list[str]:
    all_yaml_files = []
    if collect_sources_from_csvfile is not None and file is not None:
        raise Exception(
            "Please provide either --collect-sources-from-csvfile or --file, not both"
        )
    if file is not None:
        all_yaml_files.append(normalize_path(file))
    elif collect_sources_from_csvfile is not None:
        with open(Path(collect_sources_from_csvfile), "rt") as fp:
            reader = csv.DictReader(fp, fieldnames=["path"])
            for row in reader:
                all_yaml_files.append(normalize_path(row["path"]))
    return all_yaml_files


def normalize_path(file_path: str) -> str:
    return (
        file_path.replace("fbcode/", "")
        if file_path.startswith("fbcode/")
        else file_path
    )


def make_facts_batch(index_result: IndexingResult) -> FactsBatch:
    cls = type(index_result.facts)
    return FactsBatch(
        predicate=f"{_glean_name(cls)}.{_schema_version(cls)}",
        facts=[RawJSON(serialize(index_result.facts, Protocol.JSON).decode())],
    )


def index_one(file_path: str) -> IndexingResult:
    all_xrefs = fetch_all_XRefViaName(file_path)
    file_fact = facts.make_src_fact(File, file_path)
    xrefs_by_file = XRefsByFile(key=XRefsByFile_key(file=file_fact, xrefs=all_xrefs))
    return IndexingResult(
        input=file_path,
        facts=xrefs_by_file,
    )


def fetch_all_XRefViaName(file_path: str) -> list[XRefViaName]:
    all_xrefs = []
    with open(file_path, "r") as f:
        all_lines = f.readlines()
        byte_count = 0
        for line in all_lines:
            if re.match(r"^\s*_target_:", line) is not None:
                match = re.search(r"(?<=_target_:\s)[\w.]+", line)
                if match is not None:
                    all_xrefs.append(
                        XRefViaName(
                            target=Name(key=match.group(0)),
                            source=ByteSpan(
                                start=byte_count + match.start(),
                                length=match.end() - match.start(),
                            ),
                        )
                    )
            byte_count += len(line)
    return all_xrefs


if __name__ == "__main__":
    main()
