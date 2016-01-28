-module(info_bot_particle_api).

-define(ENDPOINT, <<"https://api.particle.io">>).

-export([get_devices/0, call_function/3, url_chain/2]).

get_devices() ->
  Url = url_chain([?ENDPOINT, <<"/v1/devices">>], []),
  get_json(Url).

call_function(DeviceId, FunctionName, Args) ->
  Url = url_chain([?ENDPOINT, <<"/v1/devices">>, DeviceId, FunctionName], []),
  ReqBody = {form, [{<<"arg">>, Args}]},
  case hackney:post(Url, default_headers(), ReqBody, []) of
    {ok, 200, _ResponseHeaders, RespBodyRef} ->
      {ok, RespBody} = hackney:body(RespBodyRef),
      jsx:decode(RespBody, [return_maps]);
    {error, Reason} ->
      {error, Reason}
  end.

url_chain(Segments, Opts) ->
  do_url_chain(Segments, <<>>, Opts).

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

do_url_chain([], Acc, _Opts) -> Acc;
do_url_chain([H|[]], Acc, Opts) ->
  do_url_chain([], hackney_url:make_url(Acc, H, Opts), Opts);
do_url_chain([H|T], Acc, Opts) ->
  do_url_chain(T, hackney_url:make_url(Acc, H, []), Opts).
