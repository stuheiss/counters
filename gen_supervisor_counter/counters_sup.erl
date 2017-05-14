% counters supervisor
% counters_sup:start_link/2 starts the supervisor and first counter
% counters_sup:start_child/2 starts another supervised counter
% counters_sup:stop/0 stops a counter
-module(counters_sup).
-behaviour(supervisor).

-export([start_link/2, start_child/2, init/1, stop/0]).

%% API
% Use start_link/2 in the supervisor module to start counters
% Start the supervisor and first counter
% Name is the counter name, Args is the initial state
start_link(Name, Args) ->
  io:format("~s:start_link(~p,~p)~n",[?MODULE,Name,Args]),
  supervisor:start_link({local, ?MODULE}, ?MODULE, {Name,Args}).

% Start another counter
% Call as many times as desired
start_child(Name, Args) ->
  io:format("~s:start_child(~p,~p)~n",[?MODULE,Name,Args]),
  ChildSpec=child(counters,Name,Args),
  supervisor:start_child(?MODULE, ChildSpec).

% shutdown the supervisor and all counters
stop() ->
  exit(whereis(?MODULE), normal).

% initialize a counter with a restart strategy
init({Name,Args}) ->
  io:format("~s:init(~p)~n",[?MODULE,{Name,Args}]),
  SupFlags = #{strategy => one_for_one,
               intensity => 10,
               period => 3600},
  ChildSpecList=[child(counters,Name,Args)],
  {ok, {SupFlags, ChildSpecList}}.

% helper to create one child_spec()
child(Module,Name,Args) ->
  #{id => Name,
    start => {Module, start, [{Name,Args}]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [Module]}.
