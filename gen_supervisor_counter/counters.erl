% counters client using gen_server behaviour
%%
%% call counters_sup:start_link/2 to start a supervised counter
%% call counters_sup:start_child/2 to start another supervised counter
%% call counters_sup:stop/0 to stop supervisor and its counters
%%
-module(counters).
-behavior(gen_server).

-export([read/1,tick/2]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

% for supervisor version
-export([start/1]).

%% API
read(Name) ->
  gen_server:call(Name, read).

tick(Name, N) ->
  gen_server:cast(Name, {tick, N}).

%% callbacks
% DO NOT CALL start/1 - this is the supervisor's job!
% called by counters_sup:init/1
% Name is the counter name, Args is the initial state
start({Name,Args}) ->
  io:format("~s:start(~p)~n",[?MODULE,{Name,Args}]),
  gen_server:start_link({local, Name}, ?MODULE, Args, []).

% called by gen_server:start_link from counters:start/1
init(Args) ->
  io:format("~s:init(~p)~n",[?MODULE,Args]),
  process_flag(trap_exit,true), % ensure call terminate/2 when 'EXIT' received
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

% override default callback implementations
handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  io:format("~s:terminate(~p,~p)~n",[?MODULE,Reason,State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
