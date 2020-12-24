-module(list).
-author("makst").
-export([create/1, reverse_create/1, filter/2, reverse/1, concatenate/1, flatten/1]).

create(N) -> create(N, []).
create(0, Res) -> Res;
create(N, Res) -> create(N-1, [N|Res]).

reverse_create(N) -> reverse(create(N)).

filter([], _) -> [];
filter([H | T], Limit) ->
  if
    H =< Limit -> [H|filter(T, Limit)];
    true -> filter(T, Limit)
  end.

reverse([]) -> [];
reverse(List) ->
  [H | T] = List,
  reverse(T) ++ [H].

concatenate(Lst) -> concatenate(Lst, []).
concatenate([], List) -> List;
concatenate([H|T], List) -> concatenate(T, List ++ H).

flatten_x(List) -> flatten_x(List, []).
flatten_x([], List) -> List;
flatten_x([H|T], List) ->
  if
    is_list(H) ->
      flatten_x(T, flatten_x(H) ++ List);
    true ->
      flatten_x(T, [H] ++ List)
  end.
flatten(List) -> reverse(flatten_x(List)).