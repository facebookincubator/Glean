%% @generated test fixture mirroring thrift2ast `%% Glean {...}` marker output.
%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "service", "name": "Calculator"}
-module(trpc_xlang_calc).
-codegen_source("fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift").

-export([add/1, sub/2, sub/3, async_sub/2]).

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "function", "service": "Calculator", "name": "add"}
-spec add(map()) -> {ok, integer()}.
add(_Req) -> {ok, 0}.

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "function", "service": "Calculator", "name": "sub"}
-spec sub(client, map()) -> {ok, integer()}.
sub(_Client, _Req) -> {ok, 0}.

-spec sub(client, map(), options) -> {ok, integer()}.
sub(_Client, _Req, _Opts) -> {ok, 0}.

-spec async_sub(client, map()) -> {ok, reference()}.
async_sub(_Client, _Req) -> {ok, make_ref()}.
