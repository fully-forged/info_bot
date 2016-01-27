-module(info_bot_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  info_bot_sup:start_link().

stop(_State) ->
  ok.
