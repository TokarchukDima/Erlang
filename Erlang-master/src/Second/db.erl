-module(db).
-author("makst").
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].
destroy(_) -> ok.

write(Key, Element, Db) ->
  [{Key, Element}|delete(Key, Db)].

delete(_, []) -> [];
delete(Key, [{K, _} = H | T]) ->
  if
    Key /= K -> [H|delete(Key, T)];
    true -> delete(Key, T)
  end.

read(_, []) -> {error, instance};
read(Key, [{K, Element} | T]) ->
  if
    Key == K -> {ok, Element};
    true -> read(Key, T)
  end.

match(_, []) -> [];
match(Element, [{Key, El} | T]) ->
  if
    Element == El -> [Key|match(Element, T)];
    true -> match(Element, T)
  end.
