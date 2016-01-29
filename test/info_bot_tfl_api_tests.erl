-module(info_bot_tfl_api_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("../src/info_bot_bus.hrl").

response_fixture() ->
  {ok, Content} = file:read_file("test/fixtures/sampletfl"),
  Content.

parse_response_test() ->
  {ok, #{status := Status, buses:= Buses}} = info_bot_tfl_api:parse_response(response_fixture()),
  [FirstBus | _Rest] = Buses,
  Expected = #info_bot_bus{id = 1830,
                           station_id = 47826,
                           station_name = <<"Hornsey Police Station">>,
                           number = <<"41">>,
                           destination = <<"Archway">>,
                           expected_arrival = 1422718255000},
  ?assertEqual({status, 1422718119669}, Status),
  ?assertEqual(Expected, FirstBus).

-endif.
