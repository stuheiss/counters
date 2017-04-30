% counters client using gen_server behaviour
-module(counters).
-behavior(gen_server).

-export([read/1,tick/2,stop/1]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

% for non-supervised version
-export([start_link/1]).

% for supervisor version
-export([start/1]).

%% API
% non supervised version
% Name is the counter name, Args is the initial state
start_link([Name, Args]) ->
  io:format("~s:start_link(~p)~n",[?MODULE,[Name,Args]]),
  gen_server:start_link({local, Name}, ?MODULE, Args, []);
start_link(Name) ->
  start_link([Name,0]).

% supervisor version
% This will be called by the supervisor's start_link/1
% Name is the counter name, Args is the initial state
start([Name,Args]) ->
  io:format("~s:start(~p)~n",[?MODULE,[Name,Args]]),
  gen_server:start_link({local, Name}, ?MODULE, Args, []);
start(Name) ->
  start([Name,0]).

read(Name) ->
  gen_server:call(Name, read).

tick(Name, N) ->
  gen_server:cast(Name, {tick, N}).

% only needed if the gen_server is not part of a supervision tree
stop(Name) ->
  gen_server:cast(Name, stop).

%% callbacks
init(Args) ->
  io:format("~s:init(~p)~n",[?MODULE,Args]),
  {ok, Args}.

% read
handle_call(read, _From, State) ->
  Reply=State,
  {reply, Reply, State}.

% stop (only needed if the gen_server is not part of a supervision tree)
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
