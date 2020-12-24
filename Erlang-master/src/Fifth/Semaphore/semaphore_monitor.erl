-module(semaphore_monitor).
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
  free().

free() ->
  receive
    {wait, Pid} ->
      Ref = erlang:monitor(process, Pid),
      Pid ! ok,
      busy(Pid, Ref);
    stop ->
      terminate()
  end.

busy(Pid, Ref) ->
  receive
    {signal, Pid} ->
      erlang:demonitor(Ref, [flush]),
      free();
    {'DOWN', Ref, process, Pid, _Reason} ->
      erlang:demonitor(Ref, [flush]),
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
