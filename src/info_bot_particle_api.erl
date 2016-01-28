-module(info_bot_particle_api).

-define(ENDPOINT, <<"https://api.particle.io">>).

-export([get_devices/0, call_function/3]).

get_devices() ->
  Url = hackney_url_extra:url_chain([?ENDPOINT, <<"/v1/devices">>], []),
  get_json(Url).

call_function(DeviceId, FunctionName, Args) ->
  Url = hackney_url_extra:url_chain([?ENDPOINT, <<"/v1/devices">>, DeviceId, FunctionName], []),
  ReqBody = {form, [{<<"arg">>, Args}]},
  case hackney:post(Url, default_headers(), ReqBody, []) of
    {ok, 200, _ResponseHeaders, RespBodyRef} ->
      {ok, RespBody} = hackney:body(RespBodyRef),
      jsx:decode(RespBody, [return_maps]);
    {error, Reason} ->
      {error, Reason}
  end.

get_json(Url) ->
  case hackney:get(Url, default_headers(), <<>>, []) of
    {ok, 200, _ResponseHeaders, BodyRef} ->
      {ok, Body} = hackney:body(BodyRef),
      jsx:decode(Body, [return_maps]);
    {error, Reason} ->
      {error, Reason}
  end.

default_headers() ->
  AuthHeader = <<"Bearer ">>,
  Token = list_to_binary(api_token()),
  [{<<"Authorization">>, <<AuthHeader/binary, Token/binary>>}].

api_token() ->
  os:getenv("PARTICLE_API_TOKEN").
