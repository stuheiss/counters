% counter supervisor
% counter_sup:start_link/1 starts a counter
% counter_sup:stop/0 stops a counter
-module(counter_sup).
-behaviour(supervisor).

-export([start_link/1, init/1, stop/0]).

%% API
% Use counter_sup:start_link/1 to start a counter and its supervisor
% Start the supervisor and counter
% Args is the initial state
start_link(Args) ->
  io:format("~s:start_link(~p)~n",[?MODULE,Args]),
  supervisor:start_link({global, ?MODULE}, ?MODULE, Args).

% Use counter_sup:stop/0 to stop a counter and its supervisor
stop() ->
  io:format("~s:stop()~n",[?MODULE]),
  exit(global:whereis_name(?MODULE), normal).

% initialize a counter with a restart strategy
% called by counter_sup:start_link/1
init(Args) ->
  io:format("~s:init(~p)~n",[?MODULE,Args]),
  ChildSpecList = [child(counter,Args)],
  SupFlags = #{strategy => one_for_one,
               intensity => 10,
               period => 3600},
  {ok, {SupFlags, ChildSpecList}}.

% helper to create one child_spec()
child(Module,Args) ->
  #{id => Module,
    start => {Module, start, [Args]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [Module]}.
