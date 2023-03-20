%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. mar 2023 20:15
%%%-------------------------------------------------------------------
-module(onp).
-author("adamnaumiec").

%% API
-export([onp/1]).



%% kalkulator ONP
%% +, -, *, /, sqrt, pow, sin, cos, tg, ctg
onp(Input) -> calc([], string:tokens(Input, " ")).


str_to_num(Str) ->
  case string:to_float(Str) of
    {error,no_float} -> list_to_integer(Str);
    {F,_Rest} -> F
  end.


calc([Value], []) -> Value;
calc([StackHead | StackTail], ["sqrt" | T]) -> calc([math:sqrt(StackHead) | StackTail], T);
calc([StackHead | StackTail], ["sin" | T]) -> calc([math:sin(StackHead) | StackTail], T);
calc([StackHead | StackTail], ["cos" | T]) -> calc([math:cos(StackHead) | StackTail], T);
calc([StackHead | StackTail], ["tan" | T]) -> calc([math:tan(StackHead) | StackTail], T);
calc([StackHead | StackTail], ["ctg" | T]) -> calc([1 / math:tan(StackHead) | StackTail], T);
calc([StackHead1, StackHead2 | StackTail], ["pow" | T]) -> calc([math:pow(StackHead2, StackHead1) | StackTail], T);
calc([StackHead1, StackHead2 | StackTail], ["+" | T]) -> calc([StackHead2 + StackHead1 | StackTail], T);
calc([StackHead1, StackHead2 | StackTail], ["-" | T]) -> calc([StackHead2 - StackHead1 | StackTail], T);
calc([StackHead1, StackHead2 | StackTail], ["*" | T]) -> calc([StackHead2 * StackHead1 | StackTail], T);
calc([StackHead1, StackHead2 | StackTail], ["/" | T]) when StackHead1 =:= 0.0 -> io:format("Nie mozna dzielic przez 0.0!");
calc([StackHead1, StackHead2 | StackTail], ["/" | T]) when StackHead1 =:= 0 -> io:format("Nie mozna dzielic przez 0!");
calc([StackHead1, StackHead2 | StackTail], ["/" | T]) -> calc([StackHead2 / StackHead1 | StackTail], T);
calc(Stack, [H | T]) -> calc([str_to_num(H) | Stack], T).

%% 1 + 2 * 3 - 4 / 5 + 6 === 1 2 3 * + 4 5 / - 6 + === 12.2
%% 1 + 2 + 3 + 4 + 5 + 6 * 7 === 1 2 + 3 + 4 + 5 + 6 7 * + === 57
%% ( (4 + 7) / 3 ) * (2 - 19) === 4 7 + 3 / 2 19 - * === -62.33333333333333
%% 17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1 === 17 31 4 + * 26 15 - 2 * 22 - / 1 - === Błąd dzielenia przez 0
