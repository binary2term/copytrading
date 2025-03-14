%%%-------------------------------------------------------------------
%% @doc ea1 public API
%% @end
%%%-------------------------------------------------------------------

-module(ea1_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("start args: ~p~n", [_StartArgs]),
    Dispath = cowboy_router:compile([
    {'_', [
      {"/chart", ea1_handler, []},
      {"/order1", ea1_handler, []},
      {"/order2", ea1_handler, []}
    ]}]),
    {ok, _} = cowboy:start_clear(mylistener,
      [{port, 8088}],
      #{env => #{dispatch => Dispath}}
    ),
    ea1_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
