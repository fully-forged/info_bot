-module(info_bot_device_store).
-behaviour(gen_server).

-include("info_bot_device.hrl").

-export([start_link/0, get_devices/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(REFRESH_INTERVAL, 30000).

%% API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_devices() ->
  gen_server:call(?MODULE, get_devices).

%% GEN_SERVER CALLBACKS

init([]) ->
  erlang:send_after(?REFRESH_INTERVAL, self(), refresh),
  {ok, #{devices => []}, 0}.

handle_call(get_devices, _From, State = #{devices := Devices}) ->
  {reply, Devices, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  refresh(State);
handle_info(refresh, State) ->
  erlang:send_after(?REFRESH_INTERVAL, self(), refresh),
  refresh(State);
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% PRIVATE API

refresh(State) ->
  case info_bot_particle_api:get_devices() of
    {ok, DeviceMaps} ->
      Devices = mapsToDevices(DeviceMaps),
      NewState = State#{devices := Devices},
      {noreply, NewState};
    {error, _Reason} ->
      {noreply, State}
  end.

mapsToDevices(DeviceMaps) ->
  lists:map(fun(DM) -> mapToDevice(DM) end, DeviceMaps).

mapToDevice(#{<<"id">> := Id, <<"name">> := Name, <<"connected">> := Connected}) ->
  #info_bot_device{id=Id, name=Name, connected=Connected}.
