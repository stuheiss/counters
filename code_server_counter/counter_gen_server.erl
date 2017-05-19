%%
%% Hot Code Loading!!!
%%
%% The secret sauce for never having to stop an application!!
%%
%% When new code is loaded, all new/modified functions become available.
%% Any code in a blocked state such as waiting on receive will stay blocked.
%% Any code currently executing a function such as a recursive loop will
%% continue to execute the old code.
%% Any code that is fully qualified (Module:function) will execute the new
%% code on the next function invocation. This includes recursive loops.
%%
%% Code can be replaced without losing state!
%%
%% The runtime can hold two versions of the code: current and last.
%% A code change will make the current become the last and the new the current.
%% The old last version must be purged first.
%%
%% You want to avoid executing both old and current when you initiate a code
%% change. If necessary, you can do a "hard" purge of the old code. This is
%% as it will crash the processes currently using the old code.
%%
%% Steps to code change:
%% 1. Compile the new code
%% 2. code:soft_purge(counter).
%% 3. code:load_file(counter).
%% 4. cause the loop to recurse so the new code will be executed.
%% Note, compiling with erl c(Mod) does steps 1-3.
%%
%% OTP compliant debugging
%%
%% Turn tracing on
%% sys:trace(counter_gen_server,true).
%% Turn tracing off
%% sys:trace(counter_gen_server,false).
%%
%% Get state, status
%% sys:get_state(counter_gen_server).
%% sys:get_status(counter_gen_server).
%% Set state
%% sys:replace_status(counter_gen_server,fun(_)->12345 end).
%%
-module(counter_gen_server).
-behavior(gen_server).

-export([start_link/0,read/0,tick/1,stop/0]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

% export the new reset function
%-export([reset/0]).

%% API
% use start_link() if no supervisor
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

read() ->
  gen_server:call(?MODULE, read).

tick(N) ->
  gen_server:cast(?MODULE, {tick, N}).

% the new reset function
%reset() ->
%  gen_server:cast(?MODULE, reset).

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

% the new reset callback
%handle_cast(reset, _State) ->
%  {noreply, 0};
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
