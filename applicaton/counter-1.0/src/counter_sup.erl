% counter supervisor
-module(counter_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1, stop/0]).

%% API
% Use start_link/1 in the supervisor module to start counter
% Start the supervisor and counter
% Args is the initial state
start_link(Args) ->
  io:format("~s:start_link(~p)~n",[?MODULE,Args]),
  supervisor:start_link({global, ?MODULE}, ?MODULE, Args).
% Start the counter with initial value of 0
start_link() ->
  start_link(0).

% initialize a counter with a restart strategy
init(Args) ->
  io:format("~s:init(~p)~n",[?MODULE,Args]),
  SupFlags={one_for_one, 10, 3600},            % strategy(), intensity, period
  ChildSpec={counter,                          % child_id()
             {counter, start, [Args]},         % mfargs()
             permanent,                        % restart()
             2000,                             % shutdown()
             worker,                           % worker()
             [counter]                         % modules()
            },
  {ok, {SupFlags, [ChildSpec]}}.

% shutdown the supervisor and counter
stop() ->
  io:format("~s:stop()~n",[?MODULE]),
  exit(global:whereis_name(?MODULE), normal).
