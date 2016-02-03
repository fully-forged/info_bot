-module(info_bot_screen).

-export([draw/1]).

-include("info_bot_line.hrl").

-define(SCREEN_WIDTH, 16).
-define(SEPARATOR, <<"|">>).

draw(Lines) ->
  FormattedLines = lists:map(fun(L) -> format_line(L) end, Lines),
  join_lines(FormattedLines, <<>>).

format_line(#info_bot_line{padding = left, text = Text}) ->
  LinePad = binary:copy(<<" ">>, ?SCREEN_WIDTH - byte_size(Text)),
  <<LinePad/binary, Text/binary>>;
format_line(#info_bot_line{padding = right, text = Text}) ->
  LinePad = binary:copy(<<" ">>, ?SCREEN_WIDTH - byte_size(Text)),
  <<Text/binary, LinePad/binary>>.

join_lines([], Acc) -> Acc;
join_lines([H | T], <<>>) ->
  join_lines(T, H);
join_lines([H | T], Acc) ->
  join_lines(T, <<Acc/binary, ?SEPARATOR/binary, H/binary>>).


%% format_message(Line1msg, Line2msg) ->
%%   Line1Pad = binary:copy(<<" ">>, 16 - byte_size(Line1msg)),
%%   Line2Pad = binary:copy(<<" ">>, 16 - byte_size(Line2msg)),
%%   Line1 = <<Line1msg/binary, Line1Pad/binary>>,
%%   Line2 = <<Line2Pad/binary, Line2msg/binary>>,
%%   Separator = <<"|">>,
%%   <<Line1/binary, Separator/binary, Line2/binary>>.
