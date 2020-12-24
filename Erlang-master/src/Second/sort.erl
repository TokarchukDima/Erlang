-module(sort).
-author("makst").
-import(list, [concatenate/1]).
-export([fast/1, merge/1, split/1]).

fast([]) -> [];
fast([H | T]) ->
  Left = fast([X || X <- T, X < H]),
  Right = fast([X || X <- T, X > H]),
  concatenate([Left, [H], Right]).

merge(List) when length(List) =< 1 -> List;
merge(List) ->
  {Left, Right} = split(List),
  compare(merge(Left), merge(Right)).

compare([], Right) -> Right;
compare(Left, []) -> Left;
compare([LH | LT] = Left, [RH | RT] = Right) ->
  if
    LH < RH -> [LH | compare(LT, Right)];
    true -> [RH | compare(RT, Left)]
  end.

split(List) ->
  Center = round(length(List) / 2),
  Left =  split(List, fun(I) -> I =< Center end, 1),
  Right = split(List, fun(I) -> I > Center end, 1),
  {Left, Right}.

split([], _, _) -> [];
split([H | T], Filter, Index) ->
  case Filter(Index) of
    true -> [H | split(T, Filter, Index + 1)];
    _ -> split(T, Filter, Index + 1)
  end.