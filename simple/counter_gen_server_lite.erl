-module(counter_gen_server_lite).

-export([start_link/0,read/0,tick/1,stop/0]).
-export([handle_call/3,handle_cast/2]).
-export([loop/1,call/2,cast/2]).

%%
%% Specific - API
%%
start_link() -> % use spawn_link() when there is no supervisor
  register(counter, spawn_link(fun() -> loop(0) end)).

% read the counter
read() ->
  call(counter, read).

% tick the counter
tick(N) ->
  cast(counter, {tick, N}).

% use stop() if no supervisor
stop() ->
  cast(counter, stop).

%%
%% Specific - callbacks
%%

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

%%
%% Generic (gen_server_lite)
%%
call(Server, Request) ->
  Server ! {request, Request, self()},
  receive
    {reply, Reply} ->
      Reply
  end.

cast(Server, Request) ->
  Server ! {request, Request},
  ok.

reply(From, Reply) ->
  From ! {reply, Reply}.

loop(State) ->
  receive
    {request, Request, From} ->
      {reply, Reply, NewState}=handle_call(Request, From, State),
      reply(From, Reply),
      loop(NewState);
    {request, Request} ->
      case handle_cast(Request, State) of
        {noreply, NewState} ->
          loop(NewState);
        {stop, _Reason, _State} ->
          ok
      end
  end.
