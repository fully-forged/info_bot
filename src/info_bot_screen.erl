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
