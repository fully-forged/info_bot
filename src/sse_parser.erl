-module(sse_parser).

-export([parse/1, parse_type/1, parse_data/1]).

-include("sse_event.hrl").

parse(EventBin) ->
  case binary:split(EventBin, <<"\n">>, [global, trim_all]) of
    [TypeLineBin, DataLineBin] ->
      binary:split(EventBin, <<"\n">>, [global, trim_all]),
      {event, #sse_event{type = parse_type(TypeLineBin),
                 data = parse_data(DataLineBin)}};
    [<<"event: ", Type/binary>>] ->
      {type, binary:bin_to_list(Type)};
    [<<"data: ", Data/binary>>] ->
      {data, jsx:decode(Data, [return_maps])}
  end.

parse_type(TypeBin) ->
  [<<"event: ", Type/binary>>] = binary:split(TypeBin, <<"\n">>, [global, trim_all]),
  binary:bin_to_list(Type).

parse_data(DataBin) ->
  [<<"data: ", Data/binary>>] = binary:split(DataBin, <<"\n">>, [global, trim_all]),
  jsx:decode(Data, [return_maps]).
