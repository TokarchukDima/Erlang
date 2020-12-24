%%%-------------------------------------------------------------------
%%% @author makst
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. сент. 2020 21:25
%%%-------------------------------------------------------------------
-module(area).
-author("makst").

%% API
-export([area/1]).

area({square, Side}) -> Side * Side;
area({circle, Radius}) -> math:pi() * Radius * Radius;
area({triangle, A, B, C}) ->
  S = (A + B + C) / 2,
  math:sqrt(S*(S-A)*(S-B)*(S-C));
area(_Other) -> {error, invalid_object}.