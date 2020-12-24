-module(db_server_app).
-author("makst").
-export([start/2, stop/1]).

start(_Type, _StartArgs) -> db_server_obs:start_link().

stop(_State) -> ok.
