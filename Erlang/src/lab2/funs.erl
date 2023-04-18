%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2023 15:52
%%%-------------------------------------------------------------------
-module(funs).
-author("adamnaumiec").

%% API
-export([]).



ReplaceLetters = fun(String) ->
  lists:map(
    fun(Char) ->
      case Char of
        $a -> $e;
        $e -> $o;
         _ -> Char
      end
    end,
    String
  )
end.


CountDivisibleByThree = fun(List) ->
  lists:foldl(
    fun(Elem, Acc) ->
      case Elem rem 3 of
        0 -> Acc+1;
        _ -> Acc
      end
    end,
    0,
    List
  )
end.


CountDivisibleByThree = fun(List) ->
  lists:foldl(
    fun(Elem, Acc) ->
      case Elem rem 3 of
        0 -> Acc+1;
        _ -> Acc
      end
    end,
    0,
    List
  )
end.
