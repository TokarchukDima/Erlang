-module(side_effects).
-author("makst").
-export([show_all/1, show_odd/1, show_even/1]).

show_all(N) -> print(1, N, fun(_) -> true end).
show_odd(N) -> print(1, N, fun(X) -> X rem 2 =/= 0 end).
show_even(N) -> print(1, N, fun(X) -> X rem 2 =:= 0 end).


print(To, To, Fun) ->
  print_if(To, Fun),
  {ok};
print(From, To, Fun) ->
  print_if(From, Fun),
  print(From + 1, To, Fun).


print_if(X, Fun) ->
  Var = Fun(X),
  if
    Var -> io:fwrite("Number: ~p~n", [X]);
    true -> false
  end.


