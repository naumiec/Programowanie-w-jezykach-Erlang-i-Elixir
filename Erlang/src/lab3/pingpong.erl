%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2023 15:16
%%%-------------------------------------------------------------------
-module(pingpong).
-author("adamnaumiec").

%% API
-export([start/0, stop/0, play/1]).



% utworz prcoesy ping i pong
start() ->
  register(ping, spawn(fun() -> ping_loop() end)),
  register(pong, spawn(fun() -> pong_loop() end)).


% zakoncz procesy
stop() ->
  ping ! 0,
  pong ! 0.


% wyslij N do ping
play(N) ->
  ping ! N.


ping_loop() ->
  receive
    0 -> ok;
    N -> io:format("Ping ~B~n", [N]), pong ! N-1, ping_loop()
  after
    10000 -> io:format("Ping timeout~n")
  end.


pong_loop() ->
  receive
    0 -> ok;
    N -> io:format("Pong ~B", [N]), ping ! N-1, pong_loop()
  after
    10000 -> io:format("Pong timeout~n")
  end.



