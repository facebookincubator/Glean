#!/usr/bin/env python3
# pyre-strict

import asyncio
import hashlib
import logging
import time
from dataclasses import dataclass, field
from inspect import getmodule
from itertools import groupby
from types import TracebackType
from typing import (
    AsyncIterable,
    Dict,
    Generator,
    Iterable,
    Optional,
    Sequence,
    Tuple,
    Type,
    TypeVar,
    Union,
)

from configerator.configerator_config import ConfigeratorConfig
from glean.client_config.types import ClientConfig, UseShards
from glean.glean.clients import GleanService
from glean.glean.types import (
    BatchRetry,
    DatabaseStatus,
    FactQuery,
    Failure,
    FinishResponse,
    Handle,
    JsonFactBatch,
    KickOff,
    KickOffFill,
    ListDatabases,
    Outcome,
    PredicateRef,
    QuerySyntax,
    Repo,
    Retry,
    SendJsonBatch,
    SendJsonBatchResponse,
    Success,
    UserQuery,
    UserQueryClientInfo,
    UserQueryCont,
    UserQueryEncodedResults,
    UserQueryEncoding,
    UserQueryEncodingCompact,
    UserQueryFacts,
    UserQueryOptions,
    UserQueryResults,
    Work,
    WorkFinished,
)
from glean.service.types import Service as ServiceLocator
from later import timeout
from libfb.py.asyncio.decorators import herd, memoize_forever
from libfb.py.asyncio.thrift import get_direct_client
from libfb.py.build_info import BuildInfo
from libfb.py.pwdutils import get_current_user_name
from more_itertools import flatten
from servicerouter.py3 import ClientParams, get_sr_client
from thrift.py3 import Protocol, Struct, deserialize, serialize
from typing_extensions import Final


Fact = TypeVar("Fact", bound=Struct)


client_info = UserQueryClientInfo(
    name="api-python3",
    unixname=get_current_user_name(),
    application=BuildInfo.get_build_rule(),
)


class RepoAlreadyExists(Exception):
    pass


def full_fact_name(fact: Type[Fact]) -> str:
    """Returns the schema name, predicate name, and version for a fact type."""
    return f"{_glean_name(fact)}.{_schema_version(fact)}"


def _get_client_config() -> ClientConfig:
    cfgr = ConfigeratorConfig("glean/client/client", thrift_class=ClientConfig)
    return cfgr.get()


def _fibonacci() -> Generator[int, None, None]:
    prev, cur = 1, 2
    yield prev
    while True:
        yield cur
        prev, cur = cur, prev + cur


@dataclass(frozen=True)
class GleanConfig:
    """
    An object capturing everything necessary to construct a new GleanClient.

    Useful when pickling GleanClients between processes.
    """

    repo: Repo
    client_config: ClientConfig = field(default_factory=_get_client_config)
    max_concurrent_queries: int = 200
    create_repo: bool = True
    # The limit for a single thrift call. Fetching more than this can cause the
    # server to return with an allocation limit error
    query_max_results: Optional[int] = 200
    finish_batch_timeout_sec: int = 60

    def to_client(self) -> "GleanClient":
        return GleanClient(config=self)


@dataclass(frozen=False)
class GleanClient:
    """
    A high level client wrapper for the Glean service.

    It takes care of opening a new database, serializing facts into batches,
    capturing errors, and closing the database properly.
    Note: this class is not thread-safe (yet?)

    Typical usage:

        from glean.client.py3 import GleanClient
        from glean.client_config.types import ClientConfig
        from glean.schema.python.types import Name
        from glean.service.types import Service

        async def main() -> None:
            config = ClientConfig(serv=Service(tier="glean.write.test"))
            async with await GleanClient.new("my_existing_repo") as client:
                one_fact = await client.get_by_id(Name(id=42))
                async for fact in client.get_all(Name):
                    print(fact)

            async with await GleanClient.new("my_repo", "new_hash", config) as client:
                await glean.send_batch([Name(key="my_fact_name")])

    """

    _glean_service: GleanService = field(init=False)
    config: Final[GleanConfig]
    _work_handle: Optional[str] = None
    _last_write_error: Optional[str] = None
    _query_semaphore: asyncio.BoundedSemaphore = field(init=False)
    _batch_semaphore: asyncio.BoundedSemaphore = field(
        default_factory=asyncio.BoundedSemaphore
    )
    _pending_batches: Dict[object, asyncio.Future] = field(default_factory=lambda: {})

    def __post_init__(self) -> None:
        self._query_semaphore = asyncio.BoundedSemaphore(
            value=self.config.max_concurrent_queries
        )
        self._glean_service = _get_glean_service(
            self.config.client_config, self.config.repo
        )

    async def __aenter__(self) -> "GleanClient":
        self._glean_service = await self._glean_service.__aenter__()
        return self

    async def __aexit__(
        self,
        exc_type: Optional[Type[Exception]],
        exc: Optional[Exception],
        tb: Optional[TracebackType],
    ) -> None:
        try:
            await self.finalize_db()
        finally:
            await self._glean_service.__aexit__(exc_type, exc, tb)

    @staticmethod
    async def new(
        repo_name: str,
        repo_hash: Optional[str] = None,
        client_config: Optional[ClientConfig] = None,
    ) -> "GleanClient":
        client_config = client_config if client_config else _get_client_config()
        if repo_hash is not None:
            repo = Repo(name=repo_name, hash=repo_hash)
        else:
            tmp_glean_service = _get_glean_service(client_config)
            async with tmp_glean_service as _tmp_glean_service:
                repo = await _get_latest_repo(
                    _tmp_glean_service, client_config, repo_name
                )
                if repo is None:
                    raise ValueError(
                        f"Couldn't find latest repo for '{repo_name}'"
                        " and repo_hash is missing"
                    )

        return GleanConfig(repo, client_config).to_client()

    def to_config(self) -> GleanConfig:
        return self.config

    @property
    def repo(self) -> Repo:
        return self.config.repo

    async def create_db(self) -> None:
        if not self.config.create_repo:
            return
        if not self._work_handle:
            work_handle = str(self.repo)
            kickoff_response = await self._glean_service.kickOff(
                KickOff(repo=self.repo, fill=KickOffFill(writeHandle=work_handle))
            )
            if kickoff_response.alreadyExists:
                raise RepoAlreadyExists(f"Database for {self.repo} already exists")
            self._work_handle = work_handle

        # check again to ensure we don't return 'None'
        if not self._work_handle:
            raise RuntimeError("Create db failed")

    # This expects all writing is complete before calling finalize_db.
    #
    # If there are still writes pending on the server then
    # this can throw Retry.
    async def finalize_db(self) -> None:
        logger: logging.Logger = logging.getLogger(__name__)
        logger.info("finalize_db")
        if self._work_handle is not None:
            outcome = (
                Outcome(success=Success())
                if self._last_write_error is None
                else Outcome(failure=Failure(message=self._last_write_error))
            )
            logger.info("finalize_db: " + str(self._last_write_error))
            logger.info("finalize_db: " + str(outcome))
            await self._glean_service.workFinished(
                WorkFinished(
                    work=Work(repo=self.repo, handle=self._work_handle), outcome=outcome
                )
            )
            self._work_handle = None

    # The Glean API could also throw Retry from finishBatch, catch and
    # convert to FinishResponse.retry
    async def _finishBatchRetry(self, handle: str) -> FinishResponse:
        try:
            resp = await self._glean_service.finishBatch(handle)
        except Retry as retry:
            resp = FinishResponse(retry=BatchRetry(seconds=retry.seconds))
        return resp

    async def finish_batch(self, handle: str) -> FinishResponse:
        """
        Wait for an already sent batch to be processed by the Glean server.

        Returns when the server has processed the batch, or after a
        hardcoded 60 second timeout. If this deadline has passed, the returned
        `FinishResponse` will have its `type` set to `FinishResponse.Type.retry`.

        This function keeps polling the server with a fibonacci backoff, as long
        as the server returns `FinishResponse.retry`.
        """
        resp = await self._finishBatchRetry(handle)
        try:
            async with timeout(self.config.finish_batch_timeout_sec):
                interval = iter(_fibonacci())
                while resp.type == FinishResponse.Type.retry:
                    await asyncio.sleep(max(next(interval), resp.retry.seconds))
                    resp = await self._finishBatchRetry(handle)
        except asyncio.TimeoutError:
            # deadline has passed, failure gets recorded below
            pass
        except Exception as e:
            # TODO: this should be behind debug logging
            # print(f"Offending batch: {handle[1]}")
            self._last_write_error = str(e)
            raise

        if resp.type != FinishResponse.Type.subst:
            self._last_write_error = str(resp)
        return resp

    async def _sendJsonBatch(
        self, repo: Repo, batch: SendJsonBatch
    ) -> SendJsonBatchResponse:
        interval = iter(_fibonacci())
        while True:
            try:
                send_resp = await self._glean_service.sendJsonBatch(self.repo, batch)
                return send_resp
            except Retry as retry:
                await asyncio.sleep(max(next(interval), retry.seconds))

    async def start_send_batch(self, facts: Iterable[Fact]) -> str:
        """
        Start sending a batch of facts to Glean.

        Returns when all the facts have been sent to the server, but it doesn't wait
        for the batches to be processed. The returned string is an opaque handle that
        can be passed to `finish_batch`.
        """
        await self.create_db()
        send_resp = await self._sendJsonBatch(self.repo, make_batch(facts))
        handle: Handle = send_resp.handle

        return handle

    async def send_batch(self, facts: Iterable[Fact]) -> FinishResponse:
        """
        Send a batch of facts to Glean.

        Returns when a batch has been processed by the server. See `finish_batch`
        for the exact error handling semantics.
        """
        handle = await self.start_send_batch(facts)
        return await self.finish_batch(handle)

    async def send_json_batch(self, batch: SendJsonBatch) -> FinishResponse:
        await self.create_db()
        resp = await self._sendJsonBatch(self.repo, batch)
        return await self.finish_batch(resp.handle)

    async def _run_single_angle_query(
        self,
        query_string: str,
        recursive: bool = False,
        max_results: Optional[int] = None,
        continuation: Optional[UserQueryCont] = None,
    ) -> Tuple[Iterable[bytes], Optional[UserQueryCont]]:
        query = UserQuery(
            query=query_string,
            options=UserQueryOptions(
                syntax=QuerySyntax.ANGLE,
                continuation=continuation,
                recursive=recursive,
                max_results=(
                    max_results
                    if max_results is not None
                    else self.config.query_max_results
                ),
            ),
            encodings=[
                UserQueryEncoding(
                    compact=UserQueryEncodingCompact(
                        # expand_results:
                        # If true, then when a query specifies fetching nested facts,
                        # those facts will be expanded in-place in the result facts.
                        # If false, the nested facts are returned in the nestedFacts field
                        # of UserQueryResults.
                        expand_results=recursive
                    )
                )
            ],
            client_info=client_info,
        )
        async with self._query_semaphore:
            resp = await self._glean_service.userQuery(self.repo, query)
        if resp.results.type != UserQueryEncodedResults.Type.compact:
            raise ValueError(
                f"Got unexpected results from glean for query '{query.query}'"
                f": {resp}"
            )
        return resp.results.compact.facts, resp.continuation

    async def _run_angle_query(
        self,
        query_string: str,
        recursive: bool = False,
        max_results: Optional[int] = None,
    ) -> AsyncIterable[bytes]:
        """
        Run an angle query and return thrift-compact serialized facts.
        """
        result_count = 0
        batch_max_results = min(
            filter(None, [self.config.query_max_results, max_results]), default=None
        )
        facts, continuation = await self._run_single_angle_query(
            query_string, recursive, batch_max_results
        )
        for fact in facts:
            result_count += 1
            yield fact

        while continuation is not None and (
            max_results is None or result_count < max_results
        ):
            facts, continuation = await self._run_single_angle_query(
                query_string, recursive, batch_max_results, continuation
            )
            for fact in facts:
                result_count += 1
                yield fact
                if max_results is not None and result_count >= max_results:
                    return

    def run_angle_query(
        self,
        query_string: str,
        predicate: Type[Fact],
        recursive: bool = False,
        max_results: Optional[int] = None,
    ) -> AsyncIterable[Fact]:
        """
        Run an angle query and return deserialized fact objects.

        This function fetches all results from the Glean service (subject to `max_results`
        - see below), following the server-side pagination rules. This means it might
        make more than one request to the server as the returned iterable is consumed
        (on demand).

        The `recursive` parameter controls if the facts returned are shallow (only the
        first level of `key` is not `None`, the default) or deep (all `key` fields are
        not `None`).

        `max_results` controls the amount of facts returned by this function.
        """
        return (  # pyre-ignore https://fburl.com/f2o5hbqr
            deserialize(predicate, fact, Protocol.COMPACT)
            async for fact in self._run_angle_query(
                query_string, recursive, max_results
            )
        )

    def get_all(
        self,
        predicate: Type[Fact],
        recursive: bool = False,
        max_results: Optional[int] = None,
    ) -> AsyncIterable[Fact]:
        """
        Get all facts with the given predicate.

        See run_angle_query for notes on pagination, `recursive`, and `max_results`.
        """
        query = f"{full_fact_name(predicate)} _"

        return self.run_angle_query(query, predicate, recursive, max_results)

    async def get_multiple_by_id(
        self, predicates: Sequence[Union[Fact, Tuple[Type[Fact], int]]]
    ) -> Sequence[Fact]:
        """
        Get multiple facts with the given ids.

        This function accepts fact thrift objects with their id fields set, or
        a (fact type, id) pair. The returned (deserialized) fact objects are
        in the same order as the arguments.

        For fetching individual facts, you can also use `get_by_id`, which
        automatically batches network requests (using this function).
        """
        pred_type_id_pairs = [
            predicate
            if isinstance(predicate, tuple)
            else (type(predicate), predicate.id)  # type: ignore
            for predicate in predicates
        ]

        query_params = [
            FactQuery(id=id, predicate_version=_schema_version(pred_type))
            for (pred_type, id) in pred_type_id_pairs
        ]

        async with self._query_semaphore:
            resp = await self._glean_service.userQueryFacts(
                repo=self.repo,
                q=UserQueryFacts(
                    facts=query_params,
                    encodings=[UserQueryEncoding(compact=UserQueryEncodingCompact())],
                    client_info=client_info,
                ),
            )
        if resp.results.type != UserQueryEncodedResults.Type.compact:
            raise ValueError(f"Got unexpected results from glean for fact id {id}")
        return [
            deserialize(pred_type, fact, Protocol.COMPACT)
            for ((pred_type, _id), fact) in zip(
                pred_type_id_pairs, resp.results.compact.facts
            )
        ]

    # TODO: we should only memoize per repo (ids are not unique over repos)
    @herd(ignored_args={0})
    @memoize_forever(ignored_args={0})
    async def get_by_id(self, predicate: Union[Fact, Tuple[Type[Fact], int]]) -> Fact:
        if predicate not in self._pending_batches:
            self._pending_batches[predicate] = asyncio.Future()
        result_fut = self._pending_batches[predicate]
        async with self._batch_semaphore:
            if result_fut.done():
                return await result_fut
            batches, self._pending_batches = self._pending_batches, {}
            batches = batches.items()
            results = await self.get_multiple_by_id(
                [pred for pred, _ in batches]  # type: ignore
            )
            for result, (_, fut) in zip(results, batches):
                fut.set_result(result)
        return await result_fut

    # Creating a stored predicate (store_derived_facts=True) involves writing,
    # and writing can produce a Retry exception.
    #
    # We do not need to wait on Retry for ordinary queries.
    async def _userQueryRetry(self, repo: Repo, q: UserQuery) -> UserQueryResults:
        interval = iter(_fibonacci())
        while True:
            try:
                resp = await self._glean_service.userQuery(q=q, repo=repo)
                return resp
            except Retry as retry:
                await asyncio.sleep(max(next(interval), retry.seconds))

    async def derive_and_store(self, predicate: Type[Fact]) -> None:
        """
        Directs the glean server to store the given derived predicate.

        This is equivalent to running `glean derive $predicate` from the CLI.
        """
        options = UserQueryOptions(store_derived_facts=True, syntax=QuerySyntax.ANGLE)
        query = UserQuery(
            query=f"{full_fact_name(predicate)} _",
            predicate=_glean_name(predicate),
            predicate_version=_schema_version(predicate),
            options=options,
            encodings=[UserQueryEncoding(compact=UserQueryEncodingCompact())],
            client_info=client_info,
        )
        handles = []
        async with self._query_semaphore:
            resp = await self._userQueryRetry(q=query, repo=self.repo)
        if resp.handle is not None:
            handles.append(resp.handle)
        while resp.continuation is not None:
            async with self._query_semaphore:
                resp = await self._userQueryRetry(
                    q=query(options=options(continuation=resp.continuation)),
                    repo=self.repo,
                )
            if resp.handle is not None:
                handles.append(resp.handle)

        await asyncio.gather(*[self.finish_batch(handle) for handle in handles])


def _get_glean_service(
    client_config: ClientConfig, repo: Optional[Repo] = None
) -> GleanService:

    client_params = ClientParams().setClientIdToBuildRule_DEPRECATED()
    if client_config.use_shards != UseShards.NO_SHARDS and repo is not None:
        repo_str = f"{repo.name}/{repo.hash}"
        hash = hashlib.md5(repo_str.encode("utf-8")).digest()
        shard = int.from_bytes(hash[0:8], byteorder="big") >> 1
        client_params.shardId = str(shard)

    service: ServiceLocator = client_config.serv
    if service.type == ServiceLocator.Type.tier:
        glean_service = get_sr_client(GleanService, service.tier, params=client_params)
    else:
        glean_service = get_direct_client(
            GleanService,
            host=service.hostPort.host,
            port=service.hostPort.port,
            tls=True,
            local_tls=True,
        )
    return glean_service


async def _get_latest_repo(
    glean_service: GleanService, client_config: ClientConfig, repo_name: str
) -> Optional[Repo]:
    dbs = await glean_service.listDatabases(ListDatabases())
    now = int(time.time())
    latest = None
    for db in dbs.databases:
        if (
            db.repo.name == repo_name
            and db.created_since_epoch is not None
            and (now - db.created_since_epoch > client_config.min_db_age)
            and (
                db.status == DatabaseStatus.Complete
                and db.expire_time is None
                and (
                    latest is None
                    or latest.created_since_epoch is None
                    or latest.created_since_epoch < db.created_since_epoch
                )
            )
        ):
            latest = db

    if latest is None:
        return None
    else:
        return latest.repo


def make_batch(facts: Iterable[Fact]) -> SendJsonBatch:
    facts_by_class = groupby(facts, key=lambda f: type(f))
    batch = SendJsonBatch(
        batches=[
            JsonFactBatch(
                predicate=PredicateRef(
                    name=_glean_name(cls), version=_schema_version(cls)
                ),
                facts=[serialize(fact, Protocol.JSON).decode() for fact in facts],
            )
            for cls, facts in facts_by_class
        ],
        remember=True,
    )
    return batch


def merge_batches(batches: Iterable[SendJsonBatch]) -> SendJsonBatch:
    fact_batches = flatten(b.batches for b in batches)
    by_predicate = groupby(fact_batches, key=lambda fb: fb.predicate)
    return SendJsonBatch(
        batches=[
            JsonFactBatch(
                predicate=pred, facts=list(flatten(fb.facts for fb in fact_batches))
            )
            for pred, fact_batches in by_predicate
        ],
        remember=any(b.remember for b in batches),
    )


def _glean_name(fact: Type[Fact]) -> str:
    # fact.__name__ -> glean.schema.$SCHEMA.types.$PREDICATE
    predicate = fact.__name__
    _prefix, schema, _types = fact.__module__.rsplit(".", 2)
    return f"{schema}.{predicate}"


def _schema_version(fact: Type[Fact]) -> int:
    mod = getmodule(fact)
    if mod is None:
        raise ValueError(f"Can't find where {fact} is declared")
    if not hasattr(mod, "PREDICATE_VERSIONS"):
        raise RuntimeError(f"No PREDICATE_VERSIONS mapping found for {fact}")
    return mod.PREDICATE_VERSIONS[fact.__name__]  # type: ignore
