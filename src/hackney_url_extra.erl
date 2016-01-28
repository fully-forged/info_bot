-module(hackney_url_extra).

-export([url_chain/2]).

url_chain(Segments, Opts) ->
  do_url_chain(Segments, <<>>, Opts).

do_url_chain([], Acc, _Opts) -> Acc;
do_url_chain([H|[]], Acc, Opts) ->
  do_url_chain([], hackney_url:make_url(Acc, H, Opts), Opts);
do_url_chain([H|T], Acc, Opts) ->
  do_url_chain(T, hackney_url:make_url(Acc, H, []), Opts).
