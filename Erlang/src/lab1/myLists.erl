%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. mar 2023 16:11
%%%-------------------------------------------------------------------
-module(myLists).
-author("adamnaumiec").

%% API
-export([contains/2, doubleElements/1, duplicateElements/1, sumNumbers/1, sumFloats/1, sumFloatsTail/1, sumFloatsTail/2]).



contains([], _) -> false;
contains([Head | Tail], Val) -> (Val == Head) or contains(Tail, Val).


doubleElements([]) -> [];
doubleElements([Head | Tail]) -> [2 * Head | doubleElements(Tail)].


duplicateElements([]) -> [];
duplicateElements([Head | Tail]) -> [Head, Head | duplicateElements(Tail)].


sumNumbers([]) -> 0;
sumNumbers([Head | Tail]) -> Head + sumNumbers(Tail).


sumFloats([]) -> 0;
sumFloats([Head | Tail]) -> case Head of
    X when is_float(X) -> X + sumFloats(Tail);
    _ -> sumFloats(Tail)
end.


sumFloatsTail(List) -> sumFloatsTail(List, 0).

sumFloatsTail([], Sum) -> Sum;
sumFloatsTail([Head | Tail], Sum) when is_float(Head) -> sumFloatsTail(Tail, Sum + Head);
sumFloatsTail([_ | Tail], Sum) -> sumFloatsTail(Tail, Sum).
