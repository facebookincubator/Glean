-module(example).
-export([hello/1, add/2, factorial/1]).

-include("example.hrl").

-doc "This is the hello function".
-spec hello(string()) -> ok.
hello(Name) ->
    io:format("~s, ~s!~n", [?HELLO, Name]).

-doc """
Adds two numbers
""".
-spec add(number(), number()) -> number().
add(X, Y) ->
    X + Y.

-doc """
Calculates factorial
""".
-spec factorial(number()) -> number().
factorial(0) -> 1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).
