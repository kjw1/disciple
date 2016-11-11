-module(disc_disciple_http).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {Id, Req2} = cowboy_req:binding(id, Req),
  Req3 = handle_id(Id, Req2),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.

handle_id(undefined, Req) ->
  {ok, Disciple} = supervisor:start_child(disciple_sup, #{
    id => {disc_disciple, make_ref()},
    start => {disc_disciple, start_link, []}
  }),
  NewId = disc_disciple:get_id(Disciple),
  {ok, Req2} = cowboy_req:reply(200,
    [{<<"application-type">>, <<"text/plain">>}],
    uuid:uuid_to_string(NewId, binary_standard), Req),
  Req2;
handle_id(IdBinary, Req) ->
  Id = uuid:string_to_uuid(IdBinary),
  Disciple = disciple_locator:find_disciple(Id),
  

