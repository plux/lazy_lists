%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Lazy lists
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(lazy_lists).

%%%_* Exports ==========================================================
-export([new/1, new/2]).
-export([head/1]).
-export([tail/1]).
-export([filter/2]).
-export([nth/2, nth_/2]).
-export([take/2, take_/2]).
-export([drop/2]).
-export([drop_while/2]).
-export([take_while/2, take_while_/2]).
-export([map/2]).
-export([fold/3]).
-export([decons/1]).
-export([to_list/1]).
-export([limit/2]).
-export([while/2]).
-export([zip/2]).
-export([unzip/1]).
-export([cons/2]).
-export([append/2]).
-export([sum/1]).
-export([chunk/2]).

%%%_* Generators -------------------------------------------------------
-export([seq/0, seq/1, seq/2, seq/3]).
-export([rand/0, rand/1, rand/2]).

%%%_* Types ------------------------------------------------------------
-export_type([lazy_list/0, lazy_list/1]).

%%%_* Records ==========================================================
-record(lazy_list, { gen = error(gen) :: lazy_gens:gen()
                   , acc = error(acc) :: term()
                   }).

%%%_* Types ============================================================
-opaque lazy_list() :: #lazy_list{}.
-opaque lazy_list(T) :: #lazy_list{gen :: lazy_gens:gen(T)}.

-type maybe_lazy_list(T) :: [T] | lazy_list(T) | lazy_gens:gen(T).
-type predicate(T) :: fun((T) -> boolean()).

%%%_* API ==============================================================
-spec new(maybe_lazy_list(T)) -> lazy_list(T).
new(L) when is_list(L) ->
    List = fun([H|T]) -> {H, T};
              ([])    -> fin
           end,
    new(List, L);
new(Gen) when is_function(Gen, 1) ->
    new(Gen, undefined);
new(#lazy_list{} = L) ->
    L.

-spec new(lazy_gens:gen(T), term()) -> lazy_list(T).
new(Gen, Acc) ->
    #lazy_list{gen=Gen, acc=Acc}.

-spec cons(A, lazy_list(B)) -> lazy_list(A | B).
cons(X, #lazy_list{} = L) ->
    append([X], L).

-spec append(maybe_lazy_list(A), maybe_lazy_list(B)) -> lazy_list(A | B).
append(A0, B0) ->
    Append = fun({A, B}) ->
                     case decons(A) of
                         empty ->
                             case decons(B) of
                                 empty        -> fin;
                                 {Head, Tail} -> {Head, {A, Tail}}
                             end;
                         {Head, Tail} ->
                             {Head, {Tail, B}}
                     end
             end,
    new(Append, {new(A0), new(B0)}).

-spec zip(maybe_lazy_list(A), maybe_lazy_list(B)) -> lazy_list({A, B}).
zip(A0, B0) ->
    Zip = fun({A, B}) ->
                  case {decons(A), decons(B)} of
                      {empty, _} -> fin;
                      {_, empty} -> fin;
                      {{HeadA, TailA}, {HeadB, TailB}} ->
                          {{HeadA, HeadB}, {TailA, TailB}}
                  end
          end,
    new(Zip, {new(A0), new(B0)}).

-spec unzip(lazy_list({A, B})) -> {lazy_list(A), lazy_list(B)}.
unzip(#lazy_list{} = L0) ->
    Unzip = fun({Dir, Acc}) ->
                    case {Dir, decons(Acc)} of
                        {left, {{Head, _}, L}}  -> {Head, {left, L}};
                        {right, {{_, Head}, L}} -> {Head, {right, L}};
                        {_, empty}              -> fin
                    end
            end,
    {new(Unzip, {left, L0}), new(Unzip, {right, L0})}.

-spec head(lazy_list(T)) -> T.
head(#lazy_list{} = L) ->
    {Head, _Tail} = decons(L),
    Head.

-spec tail(lazy_list(T)) -> lazy_list(T).
tail(#lazy_list{} = L) ->
    {_Head, Tail} = decons(L),
    Tail.

-spec filter(predicate(T), maybe_lazy_list(T)) -> lazy_list(T).
filter(Pred, L0) when is_function(Pred, 1) ->
    Filter = fun Filter(L) ->
                     case decons(L) of
                         {Head, Tail} ->
                             case Pred(Head) of
                                 true  -> {Head, Tail};
                                 false -> Filter(Tail)
                             end;
                         empty ->
                             fin
                     end
             end,
    new(Filter, new(L0)).

-spec map(fun((A) -> B), maybe_lazy_list(A)) -> lazy_list(B).
map(Fun, L0) when is_function(Fun, 1) ->
    Map = fun(L) ->
                  case decons(L) of
                      {Head, Tail} -> {Fun(Head), Tail};
                      empty        -> fin
                  end
          end,
    new(Map, new(L0)).

-spec fold(fun((T, Acc) -> Acc), Acc, lazy_list(T)) -> Acc.
fold(Fun, Acc, #lazy_list{} = L0) when is_function(Fun, 2) ->
    case decons(L0) of
        {Val, L} -> fold(Fun, Fun(Val, Acc), L);
        empty    -> Acc
    end.

-spec nth_(pos_integer(), lazy_list(T)) -> T.
nth_(N, #lazy_list{} = L) ->
    {Val, _} = nth(N, L),
    Val.

-spec nth(pos_integer(), lazy_list(T)) -> {T, lazy_list(T)}.
nth(N, #lazy_list{} = L0) when N > 1 ->
    decons(drop(N-1, L0)).

-spec take(non_neg_integer(), lazy_list(T)) -> {[T], lazy_list(T)}.
take(N, #lazy_list{} = L) when N >= 0 ->
    take(N, [], L).

-spec take_(non_neg_integer(), lazy_list(T)) -> [T].
take_(N, #lazy_list{} = L) when N >= 0 ->
    {List, _} = take(N, [], L),
    List.

-spec take_while(predicate(T), lazy_list(T)) -> {[T], lazy_list(T)}.
take_while(Pred, #lazy_list{} = L) when is_function(Pred, 1) ->
    take_while(Pred, [], L).

-spec take_while_(predicate(T), lazy_list(T)) -> [T].
take_while_(Pred, #lazy_list{} = L) when is_function(Pred, 1) ->
    {List, _} = take_while(Pred, [], L),
    List.

-spec drop(non_neg_integer(), lazy_list(T)) -> lazy_list(T).
drop(0, #lazy_list{} = L) ->
    L;
drop(N, #lazy_list{} = L) when N > 0 ->
    drop(N-1, tail(L)).

-spec drop_while(predicate(T), lazy_list(T)) -> lazy_list(T).
drop_while(Pred, L) ->
    {Head, Tail} = decons(L),
    case Pred(Head) of
        true  -> drop_while(Pred, Tail);
        false -> cons(Head, Tail)
    end.

-spec decons(lazy_list(T)) -> {T, lazy_list(T)} | empty.
decons(#lazy_list{gen=Gen, acc=Acc0}) ->
    case Gen(Acc0) of
        {Val, Acc} -> {Val, new(Gen, Acc)};
        fin        -> empty
    end.

-spec limit(non_neg_integer(), lazy_list(T)) -> lazy_list(T).
limit(Len0, #lazy_list{} = L0) ->
    MaxLen = fun({0, _}) ->
                     fin;
                ({Len, L}) ->
                     case decons(L) of
                         {Head, Tail} -> {Head, {Len-1, Tail}};
                         empty        -> fin
                     end
             end,
    new(MaxLen, {Len0, L0}).

-spec while(predicate(T), lazy_list(T)) -> lazy_list(T).
while(Pred, L0) ->
    While = fun(L) ->
                    case decons(L) of
                        {Head, Tail} ->
                            case Pred(Head) of
                                true -> {Head, Tail};
                                false -> fin
                            end;
                        empty ->
                            fin
                    end
            end,
    new(While, L0).

-spec chunk(pos_integer(), lazy_list(T)) -> lazy_list([T]).
chunk(N, #lazy_list{} = L0) when N > 1 ->
    Chunk = fun(L) ->
                    case take(N, L) of
                        {[], _}       -> fin;
                        {Elems, Tail} -> {Elems, Tail}
                    end
            end,
    new(Chunk, L0).

-spec to_list(lazy_list(T)) -> [T].
to_list(#lazy_list{} = L) ->
    to_list([], L).

-spec sum(lazy_list(number())) -> number().
sum(#lazy_list{} = L) ->
    fold(fun(X, Acc) -> X + Acc end, 0, L).

-spec seq() -> lazy_list(integer()).
seq() ->
    new(lazy_gens:seq()).

-spec seq(integer()) -> lazy_list(integer()).
seq(Start) ->
    new(lazy_gens:seq(Start)).

-spec seq(integer(), integer()) -> lazy_list(integer()).
seq(Start, Step) ->
    new(lazy_gens:seq(Start, Step)).

-spec seq(integer(), integer(), integer()) -> lazy_list(integer()).
seq(Start, Step, End) ->
    new(lazy_gens:seq(Start, Step, End)).

-spec rand() -> lazy_list(pos_integer()).
rand() ->
    new(lazy_gens:rand()).

-spec rand(pos_integer()) -> lazy_list(pos_integer()).
rand(N) ->
    new(lazy_gens:rand(N)).

-spec rand(pos_integer(), rand:state()) -> lazy_list(pos_integer()).
rand(N, State) ->
    new(lazy_gens:rand(N, State)).

%%%_* Internal =========================================================
take(0, Vals, L) ->
    {lists:reverse(Vals), L};
take(N, Vals, L0) ->
    case decons(L0) of
        {Val, L} -> take(N-1, [Val|Vals], L);
        empty    -> {lists:reverse(Vals), L0}
    end.

take_while(Pred, Vals, L0)  ->
    case decons(L0) of
        {Val, L} ->
            case Pred(Val) of
                true  -> take_while(Pred, [Val|Vals], L);
                false -> {lists:reverse(Vals), L}
            end;
        empty ->
            {lists:reverse(Vals), L0}
    end.

to_list(Vals, L0) ->
    case decons(L0) of
        {Val, L} -> to_list([Val|Vals], L);
        empty    -> lists:reverse(Vals)
    end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lazy_lists_test() ->
    LX = seq(1, 2),
    L0 = filter(fun(X) -> X rem 3 =/= 0 end, LX),
    L1 = filter(fun(X) -> X rem 5 =/= 0 end, L0),
    L  = map(fun(7) -> beep;
                (X) -> X
             end, L1),
    {[1, 3, 5, 7, 9], L2}       = take(5, LX),
    [1, 3, 5, 7, 9]             = take_(5, LX),
    [1, 3, 5, 7, 9]             = to_list(limit(5, LX)),
    {[11, 13, 15, 17, 19], _}   = take(5, L2),
    {[1, beep, 11, 13, 17], L3} = take(5, L),
    {[19, 23, 29, 31, 37], _}   = take(5, L3),
    ok.

init_list_test() ->
    L       = new([1,2,3]),
    [1,2,3] = to_list(L).

fold_test() ->
    L = new([1,2,3]),
    6 = fold(fun(V, Acc) -> Acc + V end, 0, L).

lazy_lists_take_while_test() ->
    L             = seq(),
    {[1,2,3], L2} = take_while(fun(X) -> X < 4 end, L),
    {[], _}       = take_while(fun(X) -> X < 4 end, L2).

fizzbuzz_test() ->
    FizzBuzz = fun(X) when X rem 3 =:= 0, X rem 5 =:= 0 -> fizzbuzz;
                  (X) when X rem 3 =:= 0                -> fizz;
                  (X) when                X rem 5 =:= 0 -> buzz;
                  (X)                                   -> X
               end,
    L0 = map(FizzBuzz, seq()),
    fizz     = nth_(9, L0),
    14       = nth_(14, L0),
    fizzbuzz = nth_(15, L0),
    fizzbuzz = nth_(999990, L0),
    fizz     = nth_(999999, L0),
    999998   = nth_(999998, L0).

cons_test() ->
    L       = seq(),
    {1, L2} = decons(L),
    [1,2,3] = take_(3, L),
    [1,2,3] = take_(3, cons(1, L2)).

seq_test() ->
    L = seq(1,1,3),
    [1,2,3] = to_list(L).

-endif.
