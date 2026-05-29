-module(example).
-behaviour(example_behaviour).
-export([hello/1, add/2, factorial/1, get_name/1, set_age/2, old_function/1, run_all/0, on_event/1]).

-include("example.hrl").

-type config() :: #{key := atom(), value := term()}.

-record(person, {name :: string(), age :: integer()}).

-deprecated([{old_function, 1, "use hello/1 instead"}]).

-doc "This is the hello function".
-spec hello(string()) -> ok.
hello(Name) ->
    io:format("~s, ~s!~n", [?HELLO, Name]).

-doc """
Adds two numbers
""".
-doc(#{since => <<"OTP 17.0">>}).
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

-spec get_name(#person{}) -> string().
get_name(#person{name = Name}) -> Name.

-spec set_age(#person{}, integer()) -> #person{}.
set_age(P, Age) -> P#person{age = Age}.

-spec old_function(term()) -> ok.
old_function(X) -> hello(X).

run_all() ->
    P = #person{name = "Alice", age = 30},
    P2 = set_age(P, 31),
    Name = get_name(P2),
    hello(Name),
    add(1, 2),
    factorial(5),
    internal_helper().

internal_helper() -> ?ADD_ONE(41).

-spec on_event(config()) -> ok.
on_event(_Event) -> ok.
