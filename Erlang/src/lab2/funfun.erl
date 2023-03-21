%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2023 15:52
%%%-------------------------------------------------------------------
-module(funfun).
-author("adamnaumiec").

%% API
-export([count_divisible_by_3/2]).



%change_a_and_o = fun([Head | Tail]) -> map() end.

sort_divisible_by_3(List) -> lists:filter(fun(X) -> X rem 3 == 0 end, List).

count_divisible_by_3([], Sum) -> Sum;
count_divisible_by_3([Head | Tail], Sum) -> count_divisible_by_3(Tail, Sum + 1).