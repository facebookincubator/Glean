-module(caller).

-export([send/3]).

send(Client, Request, Options) ->
    trpc_dep_service:call(Client, Request, Options).
