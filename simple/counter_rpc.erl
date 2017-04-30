-module(counter_rpc).
-export([start/0,stop/0,tick/1,read/0]).

%% API (no concurrent code)
start() ->
  start_server().

stop() ->
  rpc_cast(stop).

read() ->
  rpc_call(read).

tick(N) ->
  rpc_cast({tick, N}).

%% Concurrent code
rpc_call(Server, Msg) ->
  Server ! {Msg, self()},
  receive
    Reply -> Reply
  end.

rpc_call(Msg) ->
  rpc_call(whereis(counter), Msg).

rpc_cast(Server, Msg) ->
  Server ! {Msg, self()}.

rpc_cast(Msg) ->
  rpc_cast(whereis(counter), Msg).

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
