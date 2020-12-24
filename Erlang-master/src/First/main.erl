%%%-------------------------------------------------------------------
%%% @author makst
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. сент. 2020 1:07
%%%-------------------------------------------------------------------
-module(main).
-import(bool, [b_and/2, b_or/2, b_not/1, b_nand/2]).
-import(area, [area/1]).
-author("makst").
-export([get_and/2, get_area/2]).

get_and(Arg1, Arg2) -> io:write(bool:b_and(bool:b_not(bool:b_and(Arg1, Arg2)), Arg1)).
get_area(Arg1, Arg2) -> io:write(area:area({Arg1, Arg2})).


