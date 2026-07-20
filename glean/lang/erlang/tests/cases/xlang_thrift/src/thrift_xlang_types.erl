%% @generated test fixture mirroring thrift2ast `%% Glean {...}` marker output.
-module(thrift_xlang_types).
-codegen_source("fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift").

-export([const_max_retries/0]).
-export_type([color/0, timestamp/0, point/0, shape/0, not_found/0]).

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "enum", "name": "Color"}
-type color() :: red | green | blue.

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "typedef", "name": "Timestamp"}
-type timestamp() :: integer().

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "struct", "name": "Point"}
-type point() :: #{x => integer(), y => integer()}.

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "union", "name": "Shape"}
-type shape() :: {circle, integer()} | {square, integer()}.

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "exception", "name": "NotFound"}
-type not_found() :: #{message => binary()}.

%% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "constant", "name": "MAX_RETRIES"}
-spec const_max_retries() -> integer().
const_max_retries() -> 3.
