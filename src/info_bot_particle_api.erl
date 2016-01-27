-module(info_bot_particle_api).

-define(ENDPOINT, <<"https://api.particle.io">>).

-export([get_devices/0]).

get_devices() ->
  Path = <<"/v1/devices">>,
  Url = <<?ENDPOINT/binary, Path/binary>>,
  get_json(Url).

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
