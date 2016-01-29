-module(info_bot_bus_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("../src/info_bot_bus.hrl").

buses() ->
  [{info_bot_bus,187052,47826,<<"Hornsey Police Station">>,
    <<"41">>,<<"Archway">>,1454078065000},
   {info_bot_bus,187038,47826,<<"Hornsey Police Station">>,
    <<"41">>,<<"Archway">>,1454078231000}].

upcoming_test() ->
  Expected = [#{destination => <<"Archway">>,number => <<"41">>,waiting => "34 min"},
              #{destination => <<"Archway">>,number => <<"41">>,waiting => "37 min"}],
  Now = 1454076000000,
  ?assertEqual(Expected, info_bot_bus:upcoming(Now, buses())).

-endif.
