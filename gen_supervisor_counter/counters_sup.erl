% counters supervisor
-module(counters_sup).
-behaviour(supervisor).

-export([start_link/1, start_child/1, init/1, stop/0]).

%% API
% Use start_link/1 in the supervisor module to start counters
% Start the supervisor and first counter
% Name is the counter name, Args is the initial state
start_link([Name, Args]) ->
  io:format("~s:start_link(~p)~n",[?MODULE,[Name,Args]]),
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Name,Args]);
% Start the counter with initial value of 0
start_link(Name) ->
  start_link([Name, 0]).

% Start another counter
% Call as many times as desired
start_child([Name, Args]) ->
  io:format("~s:start_child(~p)~n",[?MODULE,[Name,Args]]),
  ChildSpec={Name,                             % child_id()
             {counters, start, [[Name,Args]]}, % mfargs()
             permanent,                        % restart()
             2000,                             % shutdown()
             worker,                           % worker()
             [counters]                        % modules()
            },
  supervisor:start_child(?MODULE, ChildSpec);
% Start the counter with initial value of 0
start_child(Name) ->
  start_child([Name, 0]).

% initialize a counter with a restart strategy
init([Name,Args]) ->
  io:format("~s:init(~p)~n",[?MODULE,[Name,Args]]),
  SupFlags={one_for_one, 10, 3600},            % strategy(), intensity, period
  ChildSpec={Name,                             % child_id()
             {counters, start, [[Name,Args]]}, % mfargs()
             permanent,                        % restart()
             2000,                             % shutdown()
             worker,                           % worker()
             [counters]                        % modules()
            },
  {ok, {SupFlags, [ChildSpec]}}.

% shutdown the supervisor and all counters
stop() ->
  exit(whereis(?MODULE), normal).
