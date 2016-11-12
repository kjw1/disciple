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
  {Action, Req3} = cowboy_req:binding(action, Req2),
  {Method, Req4} = cowboy_req:method(Req3),
  Req5 = handle_id(Method, Id, Action, Req4),
  {ok, Req5, State}.

terminate(_Reason, _Req, _State) ->
  ok.

handle_id(<<"POST">>, undefined, undefined, Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  #{ <<"name">> := Name} = jiffy:decode(Body, [return_maps]),
  {ok, Disciple} = supervisor:start_child(disciple_sup, #{
    id => {disc_disciple, make_ref()},
    start => {disc_disciple, start_link, [Name]}
  }),
  NewId = disc_disciple:get_id(Disciple),
  Stats = disc_disciple:get_stats(Disciple),
  Reply = Stats#{ id => uuid:uuid_to_string(NewId, binary_standard) },
  {ok, Req3} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>}],
    jiffy:encode(Reply), Req2),
  Req3;
handle_id(<<"GET">>, undefined, undefined, Req) ->
  Disciples = disc_disciple_locator:all(),
  Reply = #{ disciples => Disciples, count => length(Disciples) },
  {ok, Req2} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>}],
    jiffy:encode(Reply), Req),
  Req2;
handle_id(<<"GET">>, IdBinary, undefined, Req) ->
  Id = uuid:string_to_uuid(IdBinary),
  Disciple = disc_disciple_locator:find_disciple(Id),
  Stats = disc_disciple:get_stats(Disciple),
  {ok, Req2} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>}],
    jiffy:encode(Stats), Req),
  Req2;
handle_id(<<"POST">>, IdBinary, Action, Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  ActionParams = jiffy:decode(Body, [return_maps]),
  Id = uuid:string_to_uuid(IdBinary),
  Disciple = disc_disciple_locator:find_disciple(Id),
  ActionResult = apply_action(Action, ActionParams, Disciple),
  {ok, Req3} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>}],
    jiffy:encode(ActionResult), Req2),
  Req3.

apply_action(<<"feedback">>, #{ <<"feedback_type">> := Feedback }, Disciple) ->
  case Feedback of
    <<"encourage">> ->
      Conf = disc_disciple:give_feedback(Disciple, encourage),
      #{ <<"confidence">> => Conf };
    <<"chastise">> ->
      {Conf, Focus} = disc_disciple:give_feedback(Disciple, chastise),
      #{ <<"confidence">> => Conf, <<"focus">> => Focus };
    <<"refocus">> ->
      Focus = disc_disciple:give_feedback(Disciple, refocus),
      #{ <<"focus">> => Focus }
  end.

