-module(semaphore_link).
-author("makst").
-export([init/0, start/0, stop/0, wait/0, signal/0]).

start() ->
  register(semaphore, spawn(?MODULE, init, [])).

stop() ->
  semaphore ! stop.

wait() ->
  semaphore ! {wait, self()},
  receive ok -> ok end.

signal() ->
  semaphore ! {signal, self()}, ok.

init() ->
  process_flag(trap_exit, true),
  free().

free() ->
  receive
    {wait, Pid} ->
      try
        link(Pid),
        Pid ! ok,
        busy(Pid)
      catch
        error:noproc -> free();
        _ -> terminate()
      end;
    stop ->
      terminate()
  end.

busy(Pid) ->
  receive
    {signal, Pid} ->
      unlink(Pid),
      free();
    {'Exit', Pid, _Reason} ->
      free()
  end.

terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after
    0 -> ok
  end.
