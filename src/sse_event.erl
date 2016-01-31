-module(sse_event).

-export([process/1]).

-include("sse_event.hrl").

process(#sse_event{type = "info-bot/action", data = Data}) ->
  info_bot_device_manager:perform(Data);
process(#sse_event{type = "spark/status", data = Data}) ->
  DeviceId = binary_to_list(maps:get(<<"coreid">>, Data)),
  case maps:get(<<"data">>, Data) of
    <<"online">> ->
      info_bot_device_store:add(DeviceId);
    <<"offline">> ->
      info_bot_device_store:remove(DeviceId)
  end.
