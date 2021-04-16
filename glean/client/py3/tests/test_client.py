#!/usr/bin/env python3
# pyre-strict

from dataclasses import replace
from typing import cast

from glean.client.py3 import GleanClient
from glean.client_config.types import ClientConfig
from glean.glean.types import (
    FinishResponse,
    Outcome,
    Retry,
    SendJsonBatch,
    UserQueryCont,
    UserQueryEncodedResults,
    UserQueryResults,
    UserQueryResultsCompact,
)
from glean.schema.src.types import File
from later.unittest import TestCase, mock
from thrift.py3 import Protocol, serialize


@mock.patch("glean.client.py3.get_direct_client", mock.AsyncContextManager())
@mock.patch("glean.client.py3.get_sr_client", mock.AsyncContextManager())
class ClientFactoryTest(TestCase):
    async def test_new_defaults_to_configerator(self) -> None:
        with mock.patch("glean.client.py3._get_client_config") as get_client_config:
            async with await GleanClient.new("reponame", "hash"):
                get_client_config.assert_called_once()

    async def test_new_defaults_to_latest_repo(self) -> None:
        with mock.patch("glean.client.py3._get_latest_repo") as get_latest_repo:
            async with await GleanClient.new(
                "reponame", client_config=ClientConfig()
            ) as glean_client:
                get_latest_repo.assert_awaited_once()
                self.assertEqual(glean_client.repo, get_latest_repo.return_value)

    async def test_explicit_new(self) -> None:
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            self.assertEqual(glean_client.repo.name, "reponame")
            self.assertEqual(glean_client.repo.hash, "hash")


class ClientShardTest(TestCase):
    @mock.patch("glean.client.py3.get_sr_client")
    async def test_shard(self, fake_get_sr_client: mock.AsyncMock) -> None:
        async with await GleanClient.new("reponame", "hash", ClientConfig()):
            fake_get_sr_client.assert_called_once()
            (_name, _args, kwargs) = fake_get_sr_client.mock_calls[0]
            self.assertEqual(kwargs["params"].shardId, "3010129517480871001")


@mock.patch("glean.client.py3.get_sr_client", mock.AsyncContextManager())
class ClientTest(TestCase):
    async def test_start_send_batch(self) -> None:
        facts = [File(key="lol")]
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            glean_service = cast(mock.AsyncMock, glean_client._glean_service)
            glean_service.kickOff.return_value.alreadyExists = False
            handle = await glean_client.start_send_batch(facts)
            glean_service.sendJsonBatch.assert_awaited_once()
            batch: SendJsonBatch = glean_service.sendJsonBatch.call_args[0][1]
            self.assertEqual(batch.batches[0].predicate.name, "src.File")
            self.assertEqual(batch.batches[0].predicate.version, 1)
            self.assertEqual(batch.batches[0].facts, ['{"id":0,"key":"lol"}'])

            glean_service.finishBatch.assert_not_called()
            glean_service.finishBatch.return_value.type = FinishResponse.Type.subst
            resp = await glean_client.finish_batch(handle)
            glean_service.finishBatch.assert_awaited_once()
            self.assertEqual(resp, glean_service.finishBatch.return_value)
        glean_service.workFinished.assert_awaited_once()
        work_finished = glean_service.workFinished.call_args.args[0]
        self.assertEqual(work_finished.outcome.type, Outcome.Type.success)

    async def test_failure_is_recorded(self) -> None:
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            glean_service = cast(mock.AsyncMock, glean_client._glean_service)
            glean_service.kickOff.return_value.alreadyExists = False
            glean_service.finishBatchRetry.return_value.type = FinishResponse.Type.retry
            await glean_client.send_batch([])
        glean_service.workFinished.assert_awaited_once()
        work_finished = glean_service.workFinished.call_args.args[0]
        self.assertEqual(work_finished.outcome.type, Outcome.Type.failure)

    async def test_run_single_angle_query(self) -> None:
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            userQuery = cast(mock.AsyncMock, glean_client._glean_service).userQuery
            userQuery.return_value.results.type = UserQueryEncodedResults.Type.compact
            userQuery.return_value.results.compact.facts = [b"fact"]
            userQuery.return_value.continuation = None

            self.assertEqual(
                [b"fact"], [f async for f in glean_client._run_angle_query("haha")]
            )

    async def test_continuation_query(self) -> None:
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            userQuery = cast(mock.AsyncMock, glean_client._glean_service).userQuery
            continuation = UserQueryCont()
            userQuery.side_effect = [
                UserQueryResults(
                    continuation=continuation,
                    results=UserQueryEncodedResults(
                        compact=UserQueryResultsCompact(facts=[b"one"])
                    ),
                ),
                UserQueryResults(
                    results=UserQueryEncodedResults(
                        compact=UserQueryResultsCompact(facts=[b"two"])
                    )
                ),
            ]
            self.assertEqual(
                [b"one", b"two"],
                [f async for f in glean_client._run_angle_query("haha")],
            )
            userQuery.assert_awaited()
            self.assertEqual(len(userQuery.call_args_list), 2)
            second_call_param = userQuery.call_args_list[1][0][1]
            self.assertEqual(second_call_param.options.continuation, continuation)

    async def test_get_all(self) -> None:
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            userQuery = cast(mock.AsyncMock, glean_client._glean_service).userQuery
            f = File(key="lol")
            userQuery.return_value = UserQueryResults(
                results=UserQueryEncodedResults(
                    compact=UserQueryResultsCompact(
                        facts=[serialize(f, Protocol.COMPACT)]
                    )
                )
            )
            self.assertEqual([f], [f async for f in glean_client.get_all(File)])
            self.assertEqual(userQuery.call_args_list[0][0][1].query, "src.File.1 _")

    async def test_derive(self) -> None:
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            userQuery = cast(mock.AsyncMock, glean_client._glean_service).userQuery
            userQuery.side_effect = [
                Retry(seconds=0.11),
                Retry(seconds=0.12),
                UserQueryResults(
                    handle="one", continuation=UserQueryCont(continuation=b"cont_one")
                ),  # calls[2]
                Retry(seconds=0.13),
                Retry(seconds=0.14),
                UserQueryResults(handle="two"),  # calls[5]
            ]
            finishBatch = cast(mock.AsyncMock, glean_client._glean_service).finishBatch
            finishBatch.side_effect = [
                Retry(seconds=0.21),
                glean_client._glean_service.finishBatch,
                Retry(seconds=0.22),
                glean_client._glean_service.finishBatch,
            ]
            await glean_client.derive_and_store(File)
            userQuery.assert_awaited()
            calls = userQuery.call_args_list
            self.assertEqual(len(calls), 6)
            query = calls[2].kwargs["q"]
            self.assertEqual(query.predicate, "src.File")
            self.assertGreaterEqual(query.predicate_version, 1)
            self.assertTrue(query.options.store_derived_facts)
            query = calls[5].kwargs["q"]
            self.assertEqual(query.predicate, "src.File")
            self.assertGreaterEqual(query.predicate_version, 1)
            self.assertTrue(query.options.store_derived_facts)
            self.assertEqual(query.options.continuation.continuation, b"cont_one")

            finishBatch.assert_awaited()
            calls = finishBatch.call_args_list
            self.assertEqual(len(calls), 4)
            call_arg_set = {call.args for call in calls}
            self.assertEqual(call_arg_set, {("one",), ("two",)})

    async def test_max_results(self) -> None:
        f = File(key="lol")
        serialized_f = serialize(f, Protocol.COMPACT)
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            with mock.patch.object(glean_client, "_run_single_angle_query") as rsaq:
                rsaq.side_effect = [
                    ([serialized_f], UserQueryCont(continuation=b"lol")),
                    ([serialized_f, serialized_f], None),
                ]
                count = 0
                async for x in glean_client.get_all(File, max_results=2):
                    count += 1
                self.assertEqual(count, 2)
                rsaq.assert_called()
                self.assertEqual(len(rsaq.call_args_list), 2)
                self.assertEqual(rsaq.call_args_list[0].args[2], 2)

    async def test_recursive(self) -> None:
        f = File(key="lol")
        serialized_f = serialize(f, Protocol.COMPACT)
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            with mock.patch.object(glean_client, "_run_single_angle_query") as rsaq:
                rsaq.return_value = ([serialized_f], None)
                async for x in glean_client.get_all(File, recursive=True):
                    pass
                rsaq.assert_called()
                self.assertTrue(rsaq.call_args_list[0].args[1])

    async def test_max_results_fallback(self) -> None:
        async with await GleanClient.new(
            "reponame", "hash", ClientConfig()
        ) as glean_client:
            userQuery = cast(mock.AsyncMock, glean_client._glean_service).userQuery
            userQuery.return_value = UserQueryResults(
                results=UserQueryEncodedResults(
                    compact=UserQueryResultsCompact(facts=[b"somebytes"])
                )
            )
            batch_max_results = 10

            with mock.patch.object(
                glean_client,
                "config",
                replace(glean_client.config, query_max_results=batch_max_results),
            ):
                async for x in glean_client._run_angle_query(
                    "someq", max_results=batch_max_results + 1
                ):
                    pass

            userQuery.assert_called_once()
            self.assertEqual(
                userQuery.call_args_list[-1].args[1].options.max_results,
                batch_max_results,
            )

            with mock.patch.object(
                glean_client,
                "config",
                replace(glean_client.config, query_max_results=None),
            ):
                async for x in glean_client._run_angle_query(
                    "someq", max_results=batch_max_results + 1
                ):
                    pass

                self.assertEqual(
                    userQuery.call_args_list[-1].args[1].options.max_results,
                    batch_max_results + 1,
                )

                async for x in glean_client._run_angle_query("someq"):
                    pass

                self.assertEqual(
                    userQuery.call_args_list[-1].args[1].options.max_results, None
                )
