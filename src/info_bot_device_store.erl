-module(info_bot_device_store).
-behaviour(gen_server).

-include("info_bot_device.hrl").

-export([start_link/0, get_devices/0, add/1, remove/1]).

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

add(DeviceId) ->
  gen_server:cast(?MODULE, {add, DeviceId}).

remove(DeviceId) ->
  gen_server:cast(?MODULE, {remove, DeviceId}).

%% GEN_SERVER CALLBACKS

init([]) ->
  erlang:send_after(?REFRESH_INTERVAL, self(), refresh),
  {ok, #{devices => #{}}, 0}.

handle_call(get_devices, _From, State = #{devices := Devices}) ->
  {reply, Devices, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({add, DeviceId}, State = #{devices := Devices}) ->
  case info_bot_particle_api:get_device(DeviceId) of
    {ok, DeviceMap} ->
      Device = mapToDevice(DeviceMap),
      NewDevices = maps:put(DeviceId, Device, Devices),
      {noreply, State#{devices := NewDevices}};
    {error, _Reason} ->
      {noreply, State}
  end;
handle_cast({remove, DeviceId}, State = #{devices := Devices}) ->
  NewDevices = maps:remove(DeviceId, Devices),
  {noreply, State#{devices := NewDevices}};
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
  DevicesPropList = lists:map(fun(DM = #{<<"id">> := Id}) ->
                                  {binary_to_list(Id), mapToDevice(DM)}
                              end,
                              DeviceMaps),
  maps:from_list(DevicesPropList).

mapToDevice(#{<<"id">> := Id, <<"name">> := Name, <<"connected">> := Connected}) ->
  #info_bot_device{id=Id, name=Name, connected=Connected}.
