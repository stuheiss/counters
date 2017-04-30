-module(counter_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _Args) ->
  {ok, Env} = application:get_env(env),
  counter_sup:start_link(Env).

stop(_Data) ->
  ok.
