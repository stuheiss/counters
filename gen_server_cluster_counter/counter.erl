-module(counter).
-behavior(gen_server).

-export([start_link/0,read/0,tick/1,stop/0]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

-export([start_cluster/0]).

%% API
start_cluster() ->
  io:format("~s:start_cluster()~n",[?MODULE]),
  gen_server_cluster:start(?MODULE, ?MODULE, 0, []).

% use start_link() if no supervisor
start_link() ->
  io:format("~s:start_link()~n",[?MODULE]),
  gen_server:start_link({global, ?MODULE}, ?MODULE, 0, []).

read() ->
  gen_server:call({global,?MODULE}, read).

tick(N) ->
  gen_server:cast({global,?MODULE}, {tick, N}).

% use stop() if no supervisor
% can use gen_server:stop/1 or gen_server:cast/2
stop() ->
  gen_server:cast({global,?MODULE}, stop).

%% callbacks
init(State) ->
  io:format("~s:init(~p)~n",[?MODULE,State]),
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
  io:format("~s:handle_info(~p,~p)~n",[?MODULE,_Info,State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
