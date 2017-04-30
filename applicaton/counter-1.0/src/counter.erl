% counter client using gen_server behaviour
-module(counter).
-behavior(gen_server).

-export([read/0,tick/1,stop/0]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

% for non-supervised version
-export([start_link/0, start_link/1]).

% for supervisor version
-export([start/0,start/1]).

%% API
% non supervised version
% Args is the initial state
start_link(Args) ->
  io:format("~s:start_link(~p)~n",[?MODULE,Args]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
start_link() ->
  start_link(0).

% supervisor version
% This will be called by the supervisor's start_link/1
% Args is the initial state
start(Args) ->
  io:format("~s:start(~p)~n",[?MODULE,Args]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
start() ->
  start(0).

read() ->
  gen_server:call(?MODULE, read).

tick(N) ->
  gen_server:cast(?MODULE, {tick, N}).

% if the gen_server is not part of a supervision tree, provide stop/1
stop() ->
  gen_server:cast(?MODULE, stop).

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
