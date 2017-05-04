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
%%
-module(counter).
-export([start/0,stop/0,tick/1,read/0]).

% We MUST export loop for hot code loading to work
-export([loop/1]).

% export the new reset function
%-export([reset/0]).

start() ->
  register(counter, spawn(fun() -> loop(0) end)).

stop() ->
  counter ! {stop, self()},
  ok.

tick(N) ->
  counter ! {tick, N, self()},
  ok.

read() ->
  counter ! {read, self()},
  receive
    Reply -> Reply
  end.

% the new reset function
%reset() ->
%  counter ! {reset, self()},
%  ok.

loop(State) ->
  receive
%    {reset, _Pid} ->
%      ?MODULE:loop(0);
    {stop, _Pid} ->
      ok;
    {read, Pid} ->
      Pid ! State,
      ?MODULE:loop(State);
    {tick, N, _Pid} ->
      ?MODULE:loop(State + N)
    end.
