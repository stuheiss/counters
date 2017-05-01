-module(counters).
-export([start/1,stop/1,tick/2,read/1]).

start(Name) ->
  register(Name, spawn(fun() -> loop(0) end)).

stop(Name) ->
  Name ! {stop, self()},
  ok.

tick(Name,N) ->
  Name ! {tick, N, self()},
  ok.

read(Name) ->
  Name ! {read, self()},
  receive
    Reply -> Reply
  end.

loop(State) ->
  receive
    {stop, _PID} ->
      ok;
    {read, PID} ->
      PID ! State,
      loop(State);
    {tick, N, _PID} ->
      loop(State + N)
    end.
