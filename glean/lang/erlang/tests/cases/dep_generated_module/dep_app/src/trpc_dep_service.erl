%% @generated test fixture for dep-app xref regression coverage.
-module(trpc_dep_service).
-codegen_source("dep/service.thrift").

-export([call/3]).

call(Client, Request, Options) ->
    {Client, Request, Options}.
