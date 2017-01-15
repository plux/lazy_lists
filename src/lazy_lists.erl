%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Lazy lists
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(lazy_lists).

%%%_* Exports ==========================================================
-export([new/1, new/2]).
-export([head/1]).
-export([tail/1]).
-export([last/1]).
-export([filter/2]).
-export([partition/2]).
-export([nth/2, nth_/2]).
-export([take/2, take_/2]).
-export([drop/2]).
-export([drop_while/2]).
-export([take_while/2, take_while_/2]).
-export([map/2]).
-export([foreach/2]).
-export([fold/3]).
-export([uncons/1]).
-export([expand/1]).
-export([limit/2]).
-export([while/2]).
-export([zip/2]).
-export([unzip/1]).
-export([cons/2]).
-export([append/2]).
-export([sum/1]).
-export([length/1]).
-export([chunk/2]).
-export([every/2]).
-export([interleave/2]).
-export([join/2]).
-export([cycle/1]).
-export([dedup/1]).
-export([member/2]).
-export([max/1]).
-export([min/1]).
-export([any/2]).
-export([all/2]).
-export([find/2]).
-export([empty/0]).

%%%_* Generators -------------------------------------------------------
-export([duplicate/1, duplicate/2]).
-export([seq/0, seq/1, seq/2, seq/3]).
-export([rand/0, rand/1, rand/2]).
-export([fib/0]).
-export([perms/1]).

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
    Append = fun(#lazy_list{} = B) ->
                     case uncons(B) of
                         empty        -> fin;
                         {Head, Tail} -> {Head, Tail}
                     end;
                ({#lazy_list{} = A,  #lazy_list{} = B}) ->
                     case uncons(A) of
                         empty ->
                             case uncons(B) of
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
                  case {uncons(A), uncons(B)} of
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
                    case {Dir, uncons(Acc)} of
                        {left, {{Head, _}, L}}  -> {Head, {left, L}};
                        {right, {{_, Head}, L}} -> {Head, {right, L}};
                        {_, empty}              -> fin
                    end
            end,
    {new(Unzip, {left, L0}), new(Unzip, {right, L0})}.

-spec head(lazy_list(T)) -> T.
head(#lazy_list{} = L) ->
    {Head, _Tail} = uncons(L),
    Head.

-spec tail(lazy_list(T)) -> lazy_list(T).
tail(#lazy_list{} = L) ->
    {_Head, Tail} = uncons(L),
    Tail.

-spec last(lazy_list(T)) -> T.
last(#lazy_list{} = L) ->
    {ok, X} = fold(fun(X, _) -> {ok, X} end, empty, L),
    X.

-spec filter(predicate(T), maybe_lazy_list(T)) -> lazy_list(T).
filter(Pred, L0) when is_function(Pred, 1) ->
    Filter = fun Filter(L) ->
                     case uncons(L) of
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

-spec partition(predicate(T), maybe_lazy_list(T)) ->
                       {lazy_list(T), lazy_list(T)}.
partition(Pred, L0) when is_function(Pred, 1) ->
    {filter(Pred, L0), filter(fun(X) -> not Pred(X) end, L0)}.

-spec map(fun((A) -> B), maybe_lazy_list(A)) -> lazy_list(B).
map(Fun, L0) when is_function(Fun, 1) ->
    Map = fun(L) ->
                  case uncons(L) of
                      {Head, Tail} -> {Fun(Head), Tail};
                      empty        -> fin
                  end
          end,
    new(Map, new(L0)).

-spec foreach(fun((T) -> term()), maybe_lazy_list(T)) -> ok.
foreach(Fun, L) ->
    fold(fun(X, Acc) -> Fun(X), Acc end, ok, L).

-spec fold(fun((T, Acc) -> Acc), Acc, lazy_list(T)) -> Acc.
fold(Fun, Acc, #lazy_list{} = L0) when is_function(Fun, 2) ->
    case uncons(L0) of
        {Val, L} -> fold(Fun, Fun(Val, Acc), L);
        empty    -> Acc
    end.

-spec nth_(pos_integer(), lazy_list(T)) -> T.
nth_(N, #lazy_list{} = L) ->
    {Val, _} = nth(N, L),
    Val.

-spec nth(pos_integer(), lazy_list(T)) -> {T, lazy_list(T)}.
nth(N, #lazy_list{} = L0) when N >= 1 ->
    uncons(drop(N-1, L0)).

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
    {Head, Tail} = uncons(L),
    case Pred(Head) of
        true  -> drop_while(Pred, Tail);
        false -> cons(Head, Tail)
    end.

-spec uncons(lazy_list(T)) -> {T, lazy_list(T)} | empty.
uncons(#lazy_list{gen=Gen, acc=Acc0}) ->
    case Gen(Acc0) of
        {Val, Acc} -> {Val, new(Gen, Acc)};
        fin        -> empty
    end.

-spec limit(non_neg_integer(), lazy_list(T)) -> lazy_list(T).
limit(Len0, #lazy_list{} = L0) ->
    MaxLen = fun({0, _}) ->
                     fin;
                ({Len, L}) ->
                     case uncons(L) of
                         {Head, Tail} -> {Head, {Len-1, Tail}};
                         empty        -> fin
                     end
             end,
    new(MaxLen, {Len0, L0}).

-spec while(predicate(T), lazy_list(T)) -> lazy_list(T).
while(Pred, L0) when is_function(Pred, 1) ->
    While = fun(L) ->
                    case uncons(L) of
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

-spec chunk(pos_integer(), maybe_lazy_list(T)) -> lazy_list([T]).
chunk(N, L0) when N > 1 ->
    Chunk = fun(L) ->
                    case take(N, L) of
                        {[], _}       -> fin;
                        {Elems, Tail} -> {Elems, Tail}
                    end
            end,
    new(Chunk, new(L0)).

-spec every(pos_integer(), lazy_list(T)) -> lazy_list(T).
every(1, #lazy_list{} = L0) ->
    L0;
every(Nth, #lazy_list{} = L0) when Nth > 1 ->
    Every = fun Every({N, L}) when N rem Nth =:= 0 ->
                    case uncons(L) of
                        {Head, Tail} -> {Head, {N+1, Tail}};
                        empty        -> fin
                    end;
                Every({N, L}) ->
                    case uncons(L) of
                        {_Head, Tail} ->
                            Every({N+1, Tail});
                        empty ->
                            fin
                    end
            end,
    new(Every, {1, L0}).

-spec interleave(maybe_lazy_list(A), maybe_lazy_list(B)) -> lazy_list(A | B).
interleave(A0, B0) ->
    Interleave = fun({#lazy_list{} = A, #lazy_list{} = B}) ->
                         case uncons(A) of
                             {Head, Tail} -> {Head, {B, Tail}};
                             empty        ->
                                 case uncons(B) of
                                     {Head, Tail} -> {Head, Tail};
                                     empty        -> fin
                                 end
                         end;
                    (#lazy_list{} = L) ->
                         case uncons(L) of
                             {Head, Tail} -> {Head, Tail};
                             empty        -> fin
                         end
                 end,
    new(Interleave, {new(A0), new(B0)}).

-spec join(A, maybe_lazy_list(B)) -> lazy_list(A | B).
join(X, L0) ->
    Join = fun({odd, {Head, Tail}}) ->
                   {Head, {even, uncons(Tail)}};
              ({even, {Head, Tail}}) ->
                   {X, {odd, {Head, Tail}}};
              ({_, empty}) ->
                   fin
           end,
   new(Join, {odd, uncons(new(L0))}).

-spec duplicate(non_neg_integer(), T) -> lazy_list(T).
duplicate(Length, X) ->
    limit(Length, duplicate(X)).

-spec duplicate(T) -> lazy_list(T).
duplicate(X) ->
    new(fun(Acc) -> {X, Acc} end).

-spec cycle(maybe_lazy_list(T)) -> lazy_list(T).
cycle(L0) ->
    Cycle = fun(L) ->
                     case uncons(L) of
                         {Head, Tail} ->
                             {Head, Tail};
                         empty ->
                             case uncons(new(L0)) of
                                 {Head, Tail} ->
                                     {Head, Tail};
                                 empty ->
                                     fin
                             end
                     end
             end,
    new(Cycle, new(L0)).

-spec dedup(maybe_lazy_list(T)) -> lazy_list(T).
dedup(L0) ->
    Dedup = fun Dedup({Prev, L}) ->
                    case uncons(L) of
                        {Prev, Tail} -> Dedup({Prev, Tail});
                        {Head, Tail} -> {Head, {Head, Tail}};
                        empty        -> fin
                    end
            end,
    new(Dedup, {make_ref(), new(L0)}).

-spec expand(lazy_list(T)) -> [T].
expand(#lazy_list{} = L) ->
    expand([], L).

-spec sum(lazy_list(number())) -> number().
sum(#lazy_list{} = L) ->
    fold(fun(X, Acc) -> X + Acc end, 0, L).

-spec length(lazy_list(number())) -> number().
length(#lazy_list{} = L) ->
    fold(fun(_, Acc) -> 1 + Acc end, 0, L).

-spec member(term(), lazy_list()) -> boolean().
member(X, #lazy_list{} = L) ->
    case uncons(L) of
        {X, _}    -> true;
        {_, Tail} -> member(X, Tail);
        empty     -> false
    end.

-spec max(lazy_list(T)) -> T.
max(#lazy_list{} = L) ->
    {Head, Tail} = uncons(L),
    fold(fun(X, Acc) -> max(X, Acc) end, Head, Tail).

-spec min(lazy_list(T)) -> T.
min(#lazy_list{} = L) ->
    {Head, Tail} = uncons(L),
    fold(fun(X, Acc) -> min(X, Acc) end, Head, Tail).

-spec any(predicate(T), lazy_list(T)) -> boolean().
any(Pred, #lazy_list{} = L) ->
    case uncons(L) of
        {Head, Tail} ->
            case Pred(Head) of
                true  -> true;
                false -> any(Pred, Tail)
            end;
        empty ->
            false
    end.

-spec all(predicate(T), lazy_list(T)) -> boolean().
all(Pred, #lazy_list{} = L) ->
    case uncons(L) of
        {Head, Tail} ->
            case Pred(Head) of
                true  -> all(Pred, Tail);
                false -> false
            end;
        empty ->
            true
    end.

-spec find(predicate(T), lazy_list(T)) -> {ok, T} | error.
find(Pred, #lazy_list{} = L) ->
    case uncons(L) of
        {Head, Tail} ->
            case Pred(Head) of
                true  -> {ok, Head};
                false -> find(Pred, Tail)
            end;
        empty ->
            error
    end.

-spec empty() -> lazy_list().
empty() ->
    new([]).

%%%_* Generators -------------------------------------------------------
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

-spec fib() -> lazy_list(pos_integer()).
fib() ->
    new(lazy_gens:fib()).

-spec perms([T]) -> lazy_list([T]).
perms(L) ->
    new(lazy_gens:perms(L)).

%%%_* Internal =========================================================
take(0, Vals, L) ->
    {lists:reverse(Vals), L};
take(N, Vals, L0) ->
    case uncons(L0) of
        {Val, L} -> take(N-1, [Val|Vals], L);
        empty    -> {lists:reverse(Vals), L0}
    end.

take_while(Pred, Vals, L0)  ->
    case uncons(L0) of
        {Val, L} ->
            case Pred(Val) of
                true  -> take_while(Pred, [Val|Vals], L);
                false -> {lists:reverse(Vals), L}
            end;
        empty ->
            {lists:reverse(Vals), L0}
    end.

expand(Vals, L0) ->
    case uncons(L0) of
        {Val, L} -> expand([Val|Vals], L);
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
    [1, 3, 5, 7, 9]             = expand(limit(5, LX)),
    {[11, 13, 15, 17, 19], _}   = take(5, L2),
    {[1, beep, 11, 13, 17], L3} = take(5, L),
    {[19, 23, 29, 31, 37], _}   = take(5, L3),
    ok.

init_list_test() ->
    L       = new([1,2,3]),
    [1,2,3] = expand(L).

fold_test() ->
    L = new([1,2,3]),
    6 = fold(fun(V, Acc) -> Acc + V end, 0, L).

take_while_test() ->
    L             = seq(),
    {[1,2,3], L2} = take_while(fun(X) -> X < 4 end, L),
    {[], _}       = take_while(fun(X) -> X < 4 end, L2).

fizzbuzz_test() ->
    FizzBuzz = fun(X) when X rem 3 =:= 0, X rem 5 =:= 0 -> fizzbuzz;
                  (X) when X rem 3 =:= 0                -> fizz;
                  (X) when                X rem 5 =:= 0 -> buzz;
                  (X)                                   -> X
               end,
    L0       = map(FizzBuzz, seq()),
    fizz     = nth_(9, L0),
    14       = nth_(14, L0),
    fizzbuzz = nth_(15, L0),
    fizzbuzz = nth_(999990, L0),
    fizz     = nth_(999999, L0),
    999998   = nth_(999998, L0).

append_test() ->
    [1,2,3,4,5,6] = expand(append([1,2,3], [4,5,6])).

duplicate_test() ->
    [a,a,a,a,a,a,a] = take_(7, duplicate(a)),
    [a,a,a,a,a,a,a] = expand(duplicate(7, a)).

any_test() ->
    true  = any(fun(X) -> X > 8 end, seq()),
    false = any(fun(X) -> X > 8 end, limit(8, seq())).

all_test() ->
    true  = all(fun(X) -> X < 9 end, limit(8, seq())),
    false = all(fun(X) -> X < 8 end, limit(8, seq())).

cycle_test() ->
    [a,b,c,a,b,c,a] = take_(7, cycle([a,b,c])).

cons_test() ->
    L       = seq(),
    {1, L2} = uncons(L),
    [1,2,3] = take_(3, L),
    [1,2,3] = take_(3, cons(1, L2)),
    [1,2,3] = expand(cons(1, cons(2, cons(3, empty())))).

seq_test() ->
    L = seq(1,1,3),
    [1,2,3] = expand(L).

interleave_test() ->
    []                  = expand(interleave([], [])),
    [1,a,2,b,3,c]       = expand(interleave([1,2,3], [a,b,c])),
    [1,a,2,b,3,c,d,e,f] = expand(interleave([1,2,3], [a,b,c,d,e,f])),
    [1,a,2,b,3,c,4,5,6] = expand(interleave([1,2,3,4,5,6], [a,b,c])).


join_test() ->
    []          = expand(join(x, [])),
    [1,x,2,x,3] = expand(join(x, seq(1,1,3))).

perms_test() ->
    [[]]           = expand(perms([])),
    [[1]]          = expand(perms([1])),
    [[1,2], [2,1]] = expand(perms([1,2])),
    [[1,2,3], [1,3,2], [3,1,2], [3,2,1], [2,3,1], [2,1,3]] =
        expand(perms([2,3,1])),
    ok.

find_test() ->
    {ok, 4} = find(fun(X) -> X > 3 end, seq()),
    error   = find(fun(X) -> X > 3 end, seq(1,1,3)).

member_test() ->
    false = member(4, empty()),
    true  = member(4, seq()),
    false = member(4, seq(1,1,3)).

partition_test() ->
    {A, B} = partition(fun(X) -> X rem 2 =:= 0 end, seq()),
    [2,4,6] = take_(3, A),
    [1,3,5] = take_(3, B).

length_test() ->
    0 = ?MODULE:length(empty()),
    3 = ?MODULE:length(new([1,2,3])).

zip_test() ->
    L = zip([a,b,c], seq()),
    [{a,1}, {b,2}, {c,3}] = expand(L),
    {A, B} = unzip(L),
    [a,b,c] = expand(A),
    [1,2,3] = expand(B).

fib_test() ->
    [1,1,2,3,5,8,13] = take_(7, fib()).

head_test() ->
    ?assertError(_, head(empty())),
    1 = head(seq()).

last_test() ->
    ?assertError(_, last(empty())),
    3 = last(new([1,2,3])).

-endif.
