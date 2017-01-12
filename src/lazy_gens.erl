%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Lazy generators
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(lazy_gens).

%%%_* Exports ==========================================================
-export([seq/0, seq/1, seq/2]).
-export([rand/0, rand/1, rand/2]).
-export([fib/0]).

-export_type([gen/0, gen/1]).

%%%_* Types ============================================================
-type gen()  :: fun((Acc) -> {_, Acc} | fin).
-type gen(T) :: fun((Acc) -> {T, Acc} | fin).

%%%_* API ==============================================================
-spec seq() -> gen(integer()).
seq() ->
    seq(1, 1).

-spec seq(integer()) -> gen(integer()).
seq(Start) ->
    seq(Start, 1).

-spec seq(integer(), integer()) -> gen(integer()).
seq(Start, Step) ->
    fun(undefined) -> {Start, Start};
       (Acc)       -> {Acc+Step, Acc+Step}
    end.

-spec rand() -> gen(pos_integer()).
rand() ->
    rand(10).

-spec rand(pos_integer()) -> gen(pos_integer()).
rand(N) ->
    rand(N, rand:seed_s(exsplus)).

-spec rand(pos_integer(), rand:state()) -> gen(pos_integer()).
rand(N, State0) ->
    fun(undefined) -> rand:uniform_s(N, State0);
       (State)     -> rand:uniform_s(N, State)
    end.

fib() ->
    fun(undefined) -> {1, {0, 1}};
       ({A, B})    -> {A+B, {B, A+B}}
    end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
