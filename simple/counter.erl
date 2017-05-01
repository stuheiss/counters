-module(counter).
-export([start/0,stop/0,tick/1,read/0]).

start() ->
  register(counter, spawn(fun() -> loop(0) end)).

stop() ->
  counter ! {stop, self()},
  ok.

tick(N) ->
  counter ! {tick, N, self()},
  ok.

read() ->
  counter ! {read, self()},
  receive
    Reply -> Reply
  end.

loop(State) ->
  receive
    {stop, _Pid} ->
      ok;
    {read, Pid} ->
      Pid ! State,
      loop(State);
    {tick, N, _Pid} ->
      loop(State + N)
    end.
