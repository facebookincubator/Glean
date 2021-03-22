#!/usr/bin/env python3
# pyre-strict

import argparse
import asyncio
import logging
import sys

# These are the types used to represent the result of queries:
import glean.schema.cxx1.types as glean_cxx
from glean.client.py3 import GleanClient
from glean.client_config.types import ClientConfig
from glean.service.types import HostPort, Service


logger: logging.Logger = logging.getLogger(__name__)


async def main() -> None:
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

    options = parser.parse_args()

    config = None
    if options.tier is not None:
        config = ClientConfig(serv=Service(tier=options.tier))
    elif options.host is not None:
        config = ClientConfig(
            serv=Service(hostPort=HostPort(host=options.host, port=int(options.port)))
        )

    async with await GleanClient.new(
        options.repo_name, options.repo_hash, config
    ) as glean:
        facts = [
            fact
            async for fact in glean.run_angle_query(
                'cxx1.Name "glean_"..', glean_cxx.Name
            )
        ]
        print(facts)


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    sys.exit(asyncio.run(main()))
