%% @generated test fixture mirroring thrift2ast `%% Glean {...}` marker output.
% @codegen_source fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift

-record(server_config, {
    %% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "field", "container_kind": "struct", "container": "ServerConfig", "name": "host"}
    host :: 'undefined' | unicode:unicode_binary(),
    %% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "field", "container_kind": "struct", "container": "ServerConfig", "name": "maxRetries"}
    max_retries :: 'undefined' | integer()
}).

-record(config_error, {
    %% Glean {"file": "fbcode/glean/lang/erlang/tests/cases/xlang_thrift/example.thrift", "kind": "field", "container_kind": "exception", "container": "ConfigError", "name": "reason"}
    reason :: 'undefined' | unicode:unicode_binary()
}).
