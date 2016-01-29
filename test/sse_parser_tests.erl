-module(sse_parser_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("../src/sse_event.hrl").

response_fixture() ->
  {ok, Content} = file:read_file("test/fixtures/sse_event"),
  Content.

data() ->
  #{<<"coreid">> => <<"32002c000347343337373739">>,
    <<"data">> => <<"null">>,
    <<"published_at">> => <<"2016-01-29T11:52:13.511Z">>,
    <<"ttl">> => <<"60">>}.

parse_event_test() ->
  Expected = {event, #sse_event{type = "info-bot-next",
                                data = data()}},
  ?assertEqual(Expected, sse_parser:parse(response_fixture())).

parse_type_test() ->
  Expected = {type, "info-bot-next"},
  ?assertEqual(Expected, sse_parser:parse(<<"event: info-bot-next\n">>)).

parse_data_test() ->
  Expected = {data, data()},
  Binary = <<"data: {\"data\":\"null\",\"ttl\":\"60\",\"published_at\":\"2016-01-29T11:52:13.511Z\",\"coreid\":\"32002c000347343337373739\"}">>,
  ?assertEqual(Expected, sse_parser:parse(Binary)).

-endif.
