-module(counters_army).
-behavior(gen_server).

-export([start_link/1,read/1,tick/2,stop/1]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).
-export([demo/1]).

%% API
demo(N) ->
  io:format("Demo create ~w counters, do some computations with them, and finally stop them.~n",[N]),

  % start the counters
  {T0,Counters}=timer:tc(fun()->start_counters(N,0,[]) end),
  io:format("Started ~w counters in ~w ms.~n",[length(Counters),T0/1000]),

  % set the counters to values 1..N
  {T1,_}=timer:tc(fun()->lists:map(fun({Pid,V}) -> tick(Pid,V) end, lists:zip(Counters,lists:seq(1,N))) end),
  io:format("Set counters to [1..~w] in ~w ms.~n",[N,T1/1000]),

  % sum the counters
  {T2,V2}=timer:tc(fun()->lists:foldl(fun(Pid,Acc) -> Acc+read(Pid) end, 0, Counters) end),
  io:format("Summed all counters, got ~w in ~w ms.~n",[V2,T2/1000]),

  % set the counters to sqrt of their values
  {T3,_}=timer:tc(fun()->lists:map(fun(Pid) -> V=read(Pid),tick(Pid,-V+math:sqrt(V)) end, Counters) end),
  io:format("Set all counters to sqrt of their values in ~w ms.~n",[T3/1000]),

  % sum the counters
  {T4,V4}=timer:tc(fun()->lists:foldl(fun(Pid,Acc) -> Acc+read(Pid) end, 0, Counters) end),
  io:format("Summed all counters, got ~w in ~w ms.~n",[V4,T4/1000]),

  % stop the counters
  {T9,_}=timer:tc(fun()->lists:map(fun(Pid) -> gen_server:stop(Pid) end, Counters) end),
  io:format("Stopped ~w counters in ~w ms.~n",[length(Counters),T9/1000]),

  ok.

start_counters(N,N,Acc) -> Acc;
start_counters(N,I,Acc) ->
  {ok,Pid}=gen_server:start_link(?MODULE, 0, []),
  start_counters(N,I+1,[Pid|Acc]).

% use start_link() if no supervisor
start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, 0, []).

read(Name) ->
  gen_server:call(Name, read).

tick(Name, N) ->
  gen_server:cast(Name, {tick, N}).

% use stop() if no supervisor
stop(Name) ->
  gen_server:cast(Name, stop).

%% callbacks
init(State) ->
  {ok, State}.

% read
handle_call(read, _From, State) ->
  Reply=State,
  {reply, Reply, State}.

% stop
handle_cast(stop, State) ->
  {stop, normal, State};
% tick
handle_cast({tick, N}, State) ->
  NewState=State + N,
  {noreply, NewState}.

% default callback implementations
% can override with specific implementations

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
