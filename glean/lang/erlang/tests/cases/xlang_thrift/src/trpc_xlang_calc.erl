%% @generated test fixture mirroring thrift2ast `%% Glean {...}` marker output.
%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "service", "name": "Calculator"}
-module(trpc_xlang_calc).
-codegen_source("fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift").

-export([add/1]).

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "function", "service": "Calculator", "name": "add"}
-spec add(map()) -> {ok, integer()}.
add(_Req) -> {ok, 0}.
