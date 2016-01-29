-module(info_bot_bus).

-export([upcoming/2, get_epoch/0]).

-include("info_bot_bus.hrl").

upcoming(Now, Buses) ->
  lists:map(fun(B) ->
              #{number => B#info_bot_bus.number,
                destination => B#info_bot_bus.destination,
                waiting => waiting_time_in_mins(B#info_bot_bus.expected_arrival, Now)}
            end,
            Buses).

get_epoch() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

waiting_time_in_mins(T, Now) ->
  case (T div 1000 - Now div 1000) div 60 of
    0 -> "due";
    Mins -> integer_to_list(Mins) ++ " min"
  end.
