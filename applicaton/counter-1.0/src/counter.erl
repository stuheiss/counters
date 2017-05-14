%% counter client using gen_server behaviour
%%
%% call counter_sup:start_link/1 to start a supervisor and counter
%% call counter_sup:stop/0 to stop supervisor and its counter
%%
-module(counter).
-behavior(gen_server).

-export([read/0,tick/1]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

% supervisor callback
-export([start/1]).

%% API
read() ->
  gen_server:call({global,?MODULE}, read).

tick(N) ->
  gen_server:cast({global,?MODULE}, {tick, N}).

%% callbacks
% DO NOT CALL start/1 - this is the supervisor's job!
% called by counter_sup:init/1
% Args is the initial state
start(Args) ->
  io:format("~s:start(~p)~n",[?MODULE,Args]),
  gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

% called by gen_server:start_link from counter:start/1
init(Args) ->
  io:format("~s:init(~p)~n",[?MODULE,Args]),
  process_flag(trap_exit,true), % ensure call terminate/2 when 'EXIT' received
  {ok, Args}.

% read
handle_call(read, _From, State) ->
  Reply=State,
  {reply, Reply, State}.

% tick
handle_cast({tick, N}, State) ->
  NewState=State + N,
  {noreply, NewState}.

% override default callback implementations
handle_info(Info, State) ->
  io:format("~s:handle_info(~p,~p)~n",[?MODULE,Info,State]),
  {noreply, State}.

terminate(Reason, State) ->
  io:format("~s:terminate(~p,~p)~n",[?MODULE,Reason,State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
