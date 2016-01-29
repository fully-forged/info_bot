-module(info_bot_particle_events_handler).
-behaviour(gen_server).

-define(ENDPOINT, "api.particle.io").
-define(PREFIX, "info-bot").
-define(TIMEOUT, 30000).
-define(HEARTBEAT_DATA, <<10>>).
-define(SUCCESSFUL_CONNECTION_DATA, <<58,111,107,10,10>>).

-include("sse_event.hrl").

-export([start_link/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, Conn} = gun:open(?ENDPOINT, 443),
  monitor(process, Conn),
  Headers = info_bot_particle_api:default_headers(),
  gun:get(Conn, "/v1/devices/events/" ++ ?PREFIX, Headers),
  {ok, initial_state()}.

handle_info({gun_up, _Conn, http}, State = #{phase := idle}) ->
  {noreply, State#{phase := connected}, ?TIMEOUT};
handle_info({gun_response, _Conn, _StreamRef, fin, _Status, _Headers}, #{phase := connected}) ->
  {stop, no_data, initial_state()};
handle_info({gun_response, _Conn, _StreamRef, nofin, _Status, _Headers}, State = #{phase := connected}) ->
  {noreply, State#{phase := receiving}, ?TIMEOUT};
handle_info({gun_data, _Conn, _StreamRef, nofin, ?HEARTBEAT_DATA}, State) ->
  {noreply, State, ?TIMEOUT};
handle_info({gun_data, _Conn, _StreamRef, nofin, ?SUCCESSFUL_CONNECTION_DATA}, State) ->
  {noreply, State, ?TIMEOUT};
handle_info({gun_data, _Conn, _StreamRef, nofin, Data}, State = #{phase := receiving, event := Event}) ->
  case sse_parser:parse(Data) of
    {event, NewEvent} ->
      io:format("~w", [NewEvent]),
      {noreply, State, ?TIMEOUT};
    {type, EventType} ->
      EventWithType = Event#sse_event{type = EventType},
      NewState = State#{event := EventWithType},
      {noreply, NewState, ?TIMEOUT};
    {data, EventData} ->
      EventWithData = Event#sse_event{data = EventData},
      NewState = State#{event := EventWithData},
      io:format("~w", [EventWithData]),
      {noreply, NewState, ?TIMEOUT}
  end;
handle_info({gun_data, _Conn, _StreamRef, fin, _Data}, #{event := Event}) ->
  io:format("~w", [Event]),
  {stop, no_data, initial_state()};
handle_info({'DOWN', _Mref, process, _Conn, Reason}, _State) ->
  {stop, Reason, initial_state};
handle_info(timeout, State) ->
  {stop, timeout, State}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

initial_state() ->
  #{phase => idle, event => #sse_event{}}.
