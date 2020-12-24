%%%-------------------------------------------------------------------
%%% @author makst
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. сент. 2020 14:09
%%%-------------------------------------------------------------------
-module(demo).
-author("makst").

%% API
-export([double/1]).

double(Value) -> times(Value, 2).

times(X, Y) -> X * Y.