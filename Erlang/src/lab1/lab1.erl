%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. mar 2023 15:55
%%%-------------------------------------------------------------------
-module(lab1).
-author("adamnaumiec").

%% API
-export([add3/1, factorial/1, power/2]).



add3(X) -> X + 3.


factorial(0) -> 1;
factorial(1) -> 1;
factorial(X) -> X * factorial(X-1).


power(_, 0) -> 1;
power(X, 1) -> X;
power(X, Y) -> X * power(X, Y-1).
