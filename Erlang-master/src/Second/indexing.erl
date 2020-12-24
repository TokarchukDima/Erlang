-module(indexing).
-author("makst").
-export([read_file/1, format_index_entry/1, format_index/1, fi/1, compact_index/1, raw2doc/1, raw_read_file/1, rawdoc/1]).

raw_read_file(F) ->
  {ok, Data} = file:read_file(F),
  rawdoc(binary_to_list(Data)).

read_file(F) ->
  raw2doc(raw_read_file(F)).

rawdoc([]) -> [];
rawdoc(Data) -> rawdoc(Data, []).

rawdoc([], Line) ->
  [lists:reverse(Line)];
rawdoc([$\n|Rest], Line) ->
  [lists:reverse(Line)|rawdoc(Rest)];
rawdoc([C|Rest], Line) ->
  rawdoc(Rest, [C|Line]).

raw2doc(RawDoc) ->
  lists:append([string:tokens(Line, " \t") || Line <- RawDoc]).

compact_index([X|Rest]) when is_integer(X) ->
  compact_index([{X, X}|Rest]);
compact_index([{X, Y}, Z|Rest]) when Z =:= Y; Z =:= Y+1 ->
  compact_index([{X, Z}|Rest]);
compact_index([{_,_}=T|Rest]) ->
  [T|compact_index(Rest)];
compact_index([]) -> [].

format_index_entry({E, I}) ->
  lists:flatten([E, $\s|format_index(compact_index(I))]).

format_index(I) ->
  string:join([fi(X) || X<-I], ",").

fi({X,X}) -> io_lib:write(X);
fi({X,Y}) -> [io_lib:write(X), $-, io_lib:write(Y)].