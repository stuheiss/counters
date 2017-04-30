-module(counter).
-behavior(gen_server).

-export([start_link/0,read/0,tick/1,stop/0]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

%% API
% use start_link() if no supervisor
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

read() ->
  gen_server:call(?MODULE, read).

tick(N) ->
  gen_server:cast(?MODULE, {tick, N}).

% use stop() if no supervisor
stop() ->
  gen_server:cast(?MODULE, stop).

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
