%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2023 15:24
%%%-------------------------------------------------------------------
-module(quicksort).
-author("adamnaumiec").

%% API
-export([less_than/2, grt_eq_than/2, qs/1, random_elems/3, compare_speeds/3]).



less_than(List, Arg) -> [X || X <- List, X < Arg].


grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].


qs([]) -> [];
qs([Pivot|Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).


random_elems(N, Min, Max) -> [rand:uniform(Max - Min) + Min || _ <- lists:seq(1, N)].


compare_speeds(List, Fun1, Fun2) ->
    {Time1, _} = timer:tc(Fun1, [List]),
    {Time2, _} = timer:tc(Fun2, [List]),
    io:format("Time1: ~p, Time2: ~p~n", [Time1, Time2]).


fun_qs = fun(List) -> quicksort:qs(List) end.


