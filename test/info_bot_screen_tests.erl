-module(info_bot_screen_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("../src/info_bot_line.hrl").

message_lines() ->
  [#info_bot_line{padding = right, text = <<"Police Station">>},
   #info_bot_line{padding = left, text = <<"41">>},
   #info_bot_line{padding = left, text = <<"1 min">>},
   #info_bot_line{padding = left, text = <<"1/2">>}].

draw_test() ->
  Expected = <<"Police Station  |              41|           1 min|             1/2">>,
  ?assertEqual(Expected, info_bot_screen:draw(message_lines())).

-endif.
