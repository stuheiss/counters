-module(counter_rpc).
-export([start/0,stop/0,tick/1,read/0]).

%% API (no concurrent code)
start() ->
  start_server().

stop() ->
  rpc_cast(counter, stop).

read() ->
  rpc_call(counter, read).

tick(N) ->
  rpc_cast(counter, {tick, N}).

%% Concurrent code
rpc_call(Server, Msg) ->
  Server ! {Msg, self()},
  receive
    Reply -> Reply
  end.

rpc_cast(Server, Msg) ->
  Server ! {Msg, self()},
  ok.

start_server() ->
  register(counter, spawn(fun() -> loop(0) end)).

loop(State) ->
  receive
    {stop, _Pid} ->
      ok;
    {read, Pid} ->
      Pid ! State,
      loop(State);
    {{tick, N}, _Pid} ->
      loop(State + N)
    end.
