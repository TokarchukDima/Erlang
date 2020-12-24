%%%-------------------------------------------------------------------
%%% @author makst
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. сент. 2020 20:44
%%%-------------------------------------------------------------------
-module(bool).
-author("makst").

%% API
-export([b_and/2, b_nand/2, b_not/1, b_or/2]).

b_not(true) -> false;
  b_not(false) -> true.
b_and(true, true) -> true;
  b_and(_, _) -> false.
b_nand(true, true) -> false;
  b_nand(_, _) -> true.
b_or(true, _) -> true;
  b_or(_, true) -> true;
  b_or(false, false) -> false.