-module(info_bot_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [?CHILD(info_bot_device_store, worker),
           ?CHILD(info_bot_particle_events_handler, worker)],
  {ok, {{one_for_one, 1, 5}, Procs}}.
