-module(info_bot_device_manager).
-behaviour(gen_server).

%% API.
-export([start_link/0, process_event/1
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  counter
}).

-include("sse_event.hrl").

%% API.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_event(Event) ->
  gen_server:cast(?MODULE, {process, Event}).

%% gen_server.

init([]) ->
  {ok, #state{counter = 0}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({process, Event}, State = #state{counter = Counter}) ->
  DeviceId = maps:get(<<"coreid">>, Event#sse_event.data),
  Args = format_message(<<"Counter">>, integer_to_binary(Counter)),
  {ok, _Resp} = info_bot_particle_api:call_function(DeviceId, <<"setMessage">>, Args),
  {noreply, State#state{counter = Counter + 1}};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_message(Line1msg, Line2msg) ->
  Line1Pad = binary:copy(<<" ">>, 16 - byte_size(Line1msg)),
  Line2Pad = binary:copy(<<" ">>, 16 - byte_size(Line2msg)),
  Line1 = <<Line1msg/binary, Line1Pad/binary>>,
  Line2 = <<Line2Pad/binary, Line2msg/binary>>,
  Separator = <<"|">>,
  <<Line1/binary, Separator/binary, Line2/binary>>.
