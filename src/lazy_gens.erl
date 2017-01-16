%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Lazy generators
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(lazy_gens).

%%%_* Exports ==========================================================
-export([seq/0, seq/1, seq/2, seq/3]).
-export([rand/0, rand/1, rand/2]).
-export([fib/0]).
-export([perms/1]).
-export([read_lines/1]).

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
seq(Start, Step) when is_integer(Start),
                      is_integer(Step) ->
    fun(undefined) -> {Start, Start};
       (Acc)       -> {Acc+Step, Acc+Step}
    end.

-spec seq(integer(), integer(), integer()) -> gen(integer()).
seq(Start, Step, End) when is_integer(Start),
                           is_integer(Step),
                           is_integer(End) ->
    fun(undefined) -> {Start, Start};
       (Acc) when Step > 0, Acc >= End -> fin;
       (Acc) when Step < 0, Acc =< End -> fin;
       (Acc)                           -> {Acc+Step, Acc+Step}
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

-spec fib() -> gen(pos_integer()).
fib() ->
    fun(undefined) -> {1, {0, 1}};
       ({A, B})    -> {A+B, {B, A+B}}
    end.

%% @doc Generate all permutations of a list
%%      Implements the Steinhaus-Johnson-Trotter algorithm
%%      https://en.wikipedia.org/wiki/Steinhaus-Johnson-Trotter_algorithm
-spec perms([T]) -> gen([T]).
perms(L0) ->
    fun(undefined) ->
            L = lists:sort(L0),
            {L, array:from_list([{X, left} || X <- L])};
       (Array0) ->
            case scan_movable_largest(Array0) of
                undefined ->
                    fin;
                I ->
                    {X, _} = array:get(I, Array0),
                    Array = scan_flip_larger(X, move(Array0, I)),
                    {Ret, _} = lists:unzip(array:to_list(Array)),
                    {Ret, Array}
            end
    end.

-spec read_lines(file:name_all()) -> gen([binary()]).
read_lines(Filename) ->
    fun F(undefined) ->
            {ok, IoDevice0} = file:open(Filename, [read, binary]),
            F(IoDevice0);
        F(IoDevice) ->
            case file:read_line(IoDevice) of
                {ok, Line} ->
                    {binary:part(Line, 0, max(0, size(Line)-1)), IoDevice};
                eof ->
                    file:close(IoDevice),
                    fin
            end
    end.

%%%_* Perms helpers ----------------------------------------------------
scan_movable_largest(Array) ->
    scan_movable_largest(Array, undefined, 0).

scan_movable_largest(Array, Acc, I) ->
    case I >= array:size(Array) of
        true ->
            Acc;
        false ->
            case is_movable(Array, I) of
                true ->
                    {X, _} = array:get(I, Array),
                    case Acc of
                        undefined ->
                            scan_movable_largest(Array, I, I+1);
                        J ->
                            case X < element(1, array:get(J, Array)) of
                                true  -> scan_movable_largest(Array, Acc, I+1);
                                false -> scan_movable_largest(Array, I, I+1)
                            end
                    end;
                false ->
                    scan_movable_largest(Array, Acc, I+1)
            end
    end.

is_movable(Array, I) ->
    case array:get(I, Array) of
        {X, left} ->
             I > 0 andalso X > element(1, array:get(I-1, Array));
        {X, right} ->
             I < (array:size(Array)-1) andalso X > element(1, array:get(I+1, Array))
    end.

move(Array, I) ->
    true = is_movable(Array, I),
    case array:get(I, Array) of
        {_, left}  -> swap(Array, I, I-1);
        {_, right} -> swap(Array, I, I+1)
    end.

swap(Array, I, J) ->
    A = array:get(I, Array),
    B = array:get(J, Array),
    array:set(I, B, array:set(J, A, Array)).


scan_flip_larger(X, Array) ->
    array:map(fun(_, {Y, D}) when Y > X -> {Y, flip(D)};
                 (_, {Y, D})            -> {Y, D}
              end, Array).

flip(left)  -> right;
flip(right) -> left.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
