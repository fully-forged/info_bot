-module(info_bot_particle_events_handler).
-behaviour(gen_server).

-define(ENDPOINT, "api.particle.io").
-define(PREFIX, "info-bot").
-define(TIMEOUT, 10000).
-define(HEARTBEAT_DATA, <<10>>).
-define(SUCCESSFUL_CONNECTION_DATA, <<58,111,107,10,10>>).

-export([start_link/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, Conn} = gun:open(?ENDPOINT, 443),
  monitor(process, Conn),
  Headers = info_bot_particle_api:default_headers(),
  gun:get(Conn, "/v1/devices/events/" ++ ?PREFIX, Headers),
  {ok, idle}.

handle_info({gun_up, _Conn, http}, idle) ->
  {noreply, connected};
handle_info({gun_response, _Conn, _StreamRef, fin, _Status, _Headers}, connected) ->
  {stop, no_data, idle};
handle_info({gun_response, _Conn, _StreamRef, nofin, _Status, _Headers}, connected) ->
  {noreply, receiving};
handle_info({gun_data, _Conn, _StreamRef, nofin, ?HEARTBEAT_DATA}, receiving) ->
  {noreply, receiving};
handle_info({gun_data, _Conn, _StreamRef, nofin, ?SUCCESSFUL_CONNECTION_DATA}, receiving) ->
  {noreply, receiving};
handle_info({gun_data, _Conn, _StreamRef, nofin, Data}, receiving) ->
  io:format("~s", [Data]),
  {noreply, receiving};
handle_info({gun_data, _Conn, _StreamRef, fin, Data}, receiving) ->
  io:format("~s", [Data]),
  {stop, no_data, idle};
handle_info({'DOWN', _Mref, process, _Conn, Reason}, _State) ->
  {stop, Reason, idle}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
