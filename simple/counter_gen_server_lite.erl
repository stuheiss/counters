-module(counter_gen_server_lite).

-export([start_link/0,read/0,tick/1,stop/0]).
-export([init/1,handle_call/3,handle_cast/2]).
-export([loop/1,call/2,cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

%%
%% API (similar to gen_server conventions)
%%
% use start_link() if no supervisor
start_link() ->
  Server=spawn_link(
           fun() ->
               {ok, State}=init([]),
               loop(State) end
          ),
  register(counter, Server).

% given Args, return initial State
init(_Args) ->
  {ok, 0}.

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
%% general (gen_server_lite)
%%
call(Server, Request) ->
  Server ! {{request, Request}, self()},
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
    {{request, Request}, From} ->
      {reply, Reply, NewState}=handle_call(Request, From, State),
      reply(From, Reply),
      loop(NewState);
    {request, Request} ->
      case handle_cast(Request, State) of
        {noreply, NewState} ->
          loop(NewState);
        {stop, _Reason, _State} ->
          ok
      end;
    Unexpected ->
      io:format("loop unexpected message ~p~n",[Unexpected]),
      loop(State)
  end.

%% callbacks
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

% default callback implementations
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
