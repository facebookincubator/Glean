from __future__ import absolute_import, division, print_function, unicode_literals

import hashlib
import logging
import sys
import time
import traceback
import warnings

import glean.glean.GleanService as glean
from configerator.configerator_config import ConfigeratorConfig
from glean.client_config.ttypes import ClientConfig, UseShards
from glean.service.ttypes import Service
from libfb.py import net
from ServiceRouter import (
    ConnConfigs,
    ServiceOptions,
    ServiceRouter,
    TServiceRouterException,
)
from thrift.protocol import TSimpleJSONProtocol
from thrift.protocol.TSimpleJSONProtocol import TSimpleJSONProtocolFactory
from thrift.transport import TTransport
from thrift.util import Serializer


warnings.warn(
    "glean.client.client is deprecated. "
    "Please use glean.client.py3 instead. https://fburl.com/4wruxewc",
    category=DeprecationWarning,
    stacklevel=2,
)


# Why this isn't in libfb I don't know. There are umpteen copies of it
# across fbcode.
def deserialize_json(json, obj):
    trans = TTransport.TMemoryBuffer(json)
    prot = TSimpleJSONProtocol.TSimpleJSONProtocol(trans, obj.__class__.thrift_spec)
    obj.read(prot)


def serialize_json(struct):
    return Serializer.serialize(TSimpleJSONProtocolFactory(), struct)


GLEAN_CLIENT_CONFIG = "glean/client/client"


class GleanClient:
    """Create a client to talk to Glean.

    Typical usage:

      import glean.client.client as glean
      import glean.schema.query.cxx.ttypes as glean_query_cxx
      import glean.schema.cxx.ttypes as glean_cxx

      glean = glean.GleanClient(repo_name = "fbsource")

      decl = client.get(schema.cxx_FunctionDeclaration(id=1234))
      client.get(decl.key.signature)

    Arguments:

      repo_name   The repository to use (e.g. "fbsource")

      repo_hash   (default: None) Hash of the repository. Note only some
                  hashes are available. If this is omitted, the latest
                  complete database is used.

      config      Glean client configuration. If omitted (which is
                  preferred), the config will be fetched from
                  configerator. See
                  fbcode/configerator/structs/glean/client/client_config.thrift
    """

    def __init__(self, repo_name, repo_hash=None, config=None):
        if config is None:
            config = ConfigeratorConfig(
                GLEAN_CLIENT_CONFIG, thrift_class=ClientConfig
            ).get()

        self.config = config

        overrides = ConnConfigs(
            {
                b"thrift_protocol": b"header",
                b"thrift_transport": b"header",
                b"thrift_security": b"required",
            }
        )
        if config.host_timeout_ms is not None:
            overrides[b"sock_conntimeout"] = str(config.host_timeout_ms)
            overrides[b"processing_timeout"] = str(config.host_timeout_ms)

        service_options = ServiceOptions()

        if config.serv.getType() == Service.TIER:
            tier = config.serv.get_tier()
        else:
            tier = None

        if config.serv.getType() == Service.HOSTPORT:
            host = config.serv.get_hostPort().host
            port = config.serv.get_hostPort().port
            service_options[b"single_host"] = (
                bytes(net.fb_gethostbyname(host).encode("utf-8")),
                bytes(port.encode("utf-8")),
            )

        if repo_hash:
            self.repo = glean.Repo(repo_name, repo_hash)
        else:
            self.client = ServiceRouter().getClient2(
                glean.Client, tier, service_options, overrides
            )
            self.repo = self.getLatestRepo(repo_name)

        if config.use_shards != UseShards.NO_SHARDS:
            repo = self.repo.name + "/" + self.repo.hash
            md5hash = hashlib.md5(repo.encode("utf8")).digest()
            shard = int.from_bytes(md5hash[0:8], byteorder="big") >> 1
            service_options[b"shards"] = [shard]

        self.client = ServiceRouter().getClient2(
            glean.Client, tier, service_options, overrides
        )

    def getLatestRepo(self, repo_name):
        """Get the latest database available for the given repository name."""
        dbs = self.client.listDatabases()
        now = int(time.time())
        latest = None
        for db in dbs.databases:
            if (
                db.repo.name == repo_name
                and (now - db.created_since_epoch > self.config.min_db_age)
                and (
                    db.status == glean.DatabaseStatus.Complete
                    and db.expire_time is None
                    and (
                        latest is None
                        or latest.created_since_epoch < db.created_since_epoch
                    )
                )
            ):
                latest = db

        if latest is None:
            return None
        else:
            return latest.repo

    def get(self, object, recursive=False):
        """Fetch a fact from Glean.

        arguments:

           fact   A fact object, with the id field set to the fact id to
                  fetch, key=None (or missing), and value=None (or missing).

        On return, the key and value will be filled in with the
        appropriate fact data from Glean.

        """
        if not hasattr(object, "id"):
            raise Exception("get() must be passed a fact object")
        if (
            hasattr(object, "key")
            and object.key is not None
            or hasattr(object, "value")
            and object.value is not None
        ):
            print(object)
            raise Exception(
                "in get(fact), fact.key and fact.value " + "must be missing or None"
            )
        result = self.client.userQueryFacts(
            self.repo,
            glean.UserQueryFacts(
                facts=[glean.FactQuery(id=object.id, recursive=recursive)],
                # Python's Thrift JSON serialization is broken:
                options=glean.UserQueryOptions(no_base64_binary=True),
            ),
        )
        deserialize_json(result.facts[0], object)
        return object

    def jsonQueryWithContinuation(self, cls, json, recursive=False, options=None):
        # module = 'glean.schema.query.clang.pp.ttypes'
        # we want: namespace = 'clang.pp'
        namespace = ".".join(cls.__module__.split(".")[3:-1])
        predicate = namespace + "." + cls.__name__
        if options is None:
            options = glean.UserQueryOptions()
        options.no_base64_binary = True
        options.recursive = recursive
        results = self.client.userQuery(
            self.repo, glean.UserQuery(predicate, json, options=options)
        )

        facts = []
        schema_module = sys.modules["glean.schema." + namespace + ".ttypes"]
        resultcls = getattr(schema_module, cls.__name__)
        for fact in results.facts:
            obj = resultcls()
            deserialize_json(fact, obj)
            facts.append(obj)

        return facts, results.continuation

    def jsonQuery(self, cls, json, recursive=False, options=None):
        facts, _ = self.jsonQueryWithContinuation(cls, json, recursive, options)
        return facts

    def query(self, object, recursive=False, options=None):
        """Query Glean for facts matching a given pattern.

        arguments:
          object
              A query object.  This should be an instance of a Thrift
              object in the glean.schema.query namespace.
              For documentation ee:
                 https://our.intern.facebook.com/intern/wiki/Glean/Query/
          recursive
              (True/False, default=False) Whether to expand facts
              recursively in the result.
          options
              (UserQueryOptions, default=None) Used to set options
              for passing to the Glean query engine. For example, we
              can set the maximum number of results with
                options=UserQueryOptions(max_results=100)

        returns:
          A list of matching facts, with all fields populated, and
          nested facts populated as described above.

        """
        json = serialize_json(object)
        return self.jsonQuery(object.__class__, json, recursive, options)

    def queryRec(self, object):
        """Query Glean for facts matching a given pattern. Like query(), but
        recursively expands nested facts in the result.

        returns:
          A list of matching facts, with all fields populated, and
          nested facts populated recursively.

        """
        return self.query(object, recursive=True)

    def jsonQueryPaged(
        self, cls, query_json, limit=None, page_size=500, recursive=False
    ):
        """Queries Glean retrieving the results in multiple chained requests.

        arguments:
          cls
            The Glean query class type of the fact type.
          query_json
            The JSON representation of the query.
          limit
            Optionally, a limit on the number of results returned.
          page_size
            The number of results requested from Glean in each network call.

        returns:
          A generator for the facts queried.
        """

        def query_page(query_size, continuation):
            if limit is None:
                query_size = None
            else:
                query_size = min(limit - fact_count, page_size)

            while True:
                try:
                    facts, continuation = self.jsonQueryWithContinuation(
                        cls,
                        query_json,
                        recursive=recursive,
                        options=glean.UserQueryOptions(
                            max_results=query_size, continuation=continuation
                        ),
                    )
                    return facts, query_size, continuation
                except TServiceRouterException:
                    logging.getLogger().warning(
                        "Exception while querying Glean, will retry\n"
                        + traceback.format_exc()
                    )
                    time.sleep(1)  # Wait one second if we got a timeout

        fact_count = 0
        query_size = None
        continuation = None

        facts, query_size, continuation = query_page(query_size, continuation)
        fact_count += len(facts)
        for fact in facts:
            yield fact

        while continuation is not None and (limit is None or fact_count < limit):
            facts, query_size, continuation = query_page(query_size, continuation)
            fact_count += len(facts)
            for fact in facts:
                yield fact
