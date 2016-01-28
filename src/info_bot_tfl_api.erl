-module(info_bot_tfl_api).

-export([parse_response/1, get_buses/0]).

-record(status, {last_update}).

-define(ENDPOINT, <<"http://countdown.api.tfl.gov.uk/interfaces/ura">>).
-define(STOPS, ["47826", "47780", "51174", "51311"]).
-define(ATTRIBUTES, ["StopCode1", "StopPointName", "LineName",
                     "DestinationText", "EstimatedTime", "TripID"]).

-include("info_bot_bus.hrl").

get_buses() ->
  case hackney:get(api_url(), [], <<>>, []) of
    {ok, 200, _RespHeaders, Client} ->
      {ok, Body} = hackney:body(Client),
      parse_response(Body);
    {error, Reason} ->
      {error, Reason}
  end.

api_url() ->
  Path = <<"/instant_V1">>,
  QsParams = [{<<"StopCode1">>, string:join(?STOPS, ",")},
              {<<"ReturnList">>, string:join(?ATTRIBUTES, ",")}],
  hackney_url:make_url(?ENDPOINT, Path, QsParams).

parse_response(Data) ->
  [Status | BusData] = binary:split(Data, <<"\r\n">>, [global]),
  {ok, #{status => parse_status(Status),
         buses => parse_buses(BusData)}}.

parse_status(Status) ->
  [_ResponseType, _Version, LastUpdate] = jsx:decode(Status),
  #status{last_update = LastUpdate}.

parse_buses(Buses) ->
  lists:map(fun(Bus) ->
                [_ResponseType, StationName, StationId,
                 Number, Destination, Id, ExpectedArrival] = jsx:decode(Bus),
                #info_bot_bus{id = Id,
                              station_id = binary_to_integer(StationId),
                              station_name = StationName,
                              number = Number,
                              destination = Destination,
                              expected_arrival = ExpectedArrival}
            end,
            Buses).
