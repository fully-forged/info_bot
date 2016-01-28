-module(info_bot_particle_api_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

url_chain_test() ->
  Expected = <<"foo/bar/baz">>,
  Segments = [<<"foo">>, <<"bar">>, <<"baz">>],
  ?assertEqual(Expected,info_bot_particle_api:url_chain(Segments, [])).

url_chain_with_opts_test() ->
  Expected = <<"foo/bar/baz?a=b">>,
  Segments = [<<"foo">>, <<"bar">>, <<"baz">>],
  Opts = [{<<"a">>, <<"b">>}],
  ?assertEqual(Expected,info_bot_particle_api:url_chain(Segments, Opts)).

-endif.
