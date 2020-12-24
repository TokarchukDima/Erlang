-module(default_higher_order_functions).
-author("makst").
-export([reverse/1]).

reverse(L) ->
  reverse(L, []).

reverse([], Acc) ->
  Acc;
reverse([H|T], Acc) ->
  reverse(T, [H|Acc]).
