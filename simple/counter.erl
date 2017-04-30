-module(counter).
-export([start/0,stop/0,tick/1,read/0]).

start() ->
  register(counter, spawn(fun() -> loop(0) end)).

stop() ->
  whereis(counter) ! {stop, self()}.

tick(N) ->
  whereis(counter) ! {tick, N, self()}.

read() ->
  whereis(counter) ! {read, self()},
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
