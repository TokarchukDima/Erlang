-module(db_lib).
-author("makst").
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() ->
  [].

destroy(X) when is_list(X) ->
  ok.

write(Key, Element, Db) ->
  [{Key, Element}|lists:keydelete(Key, 1, Db)].

delete(Key, Db) ->
  lists:keydelete(Key, 1, Db).

read(Key, Db) ->
  read(lists:keyfind(Key, 1, Db)).
read(false) ->
  {error, instance};
read({_, Element}) ->
  {ok, Element}.

match(Element, Db) ->
  [Key || {Key, X} <- Db, X =:= Element].