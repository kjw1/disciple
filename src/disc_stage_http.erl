-module(disc_stage_http).
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
  {Method, Req3} = cowboy_req:method(Req2),
  Req4 = handle_id(Method, Id, Req3),
  {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
  ok.

handle_id(<<"POST">>, undefined, Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  #{ <<"description">> := Description,
     <<"difficulty">> := Difficulty,
     <<"success">> := #{
       <<"message">> := SMessage,
       <<"consequences">> := SCons
     },
     <<"failure">> := #{
       <<"message">> := FMessage,
       <<"consequences">> := FCons
     }
  } = jiffy:decode(Body, [return_maps]),
  Success = disc_stage:new_outcome(SMessage, translate_cons(SCons)),
  Failure = disc_stage:new_outcome(FMessage, translate_cons(FCons)),
  Stage = disc_stage:new(Description, Difficulty, Success, Failure),
  disc_stage:save(Stage),
  StageReply = stage_to_map(Stage),
  {ok, Req3} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>}],
    jiffy:encode(StageReply), Req2),
  Req3;
handle_id(<<"GET">>, Id, Req) ->
  Uuid = uuid:string_to_uuid(Id),
  Stage = disc_stage:get_stage(Uuid),
  StageReply = stage_to_map(Stage),
  {ok, Req2} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>}],
    jiffy:encode(StageReply), Req),
  Req2.

translate_cons(InCons) ->
  lists:map(fun(#{ <<"stat">> := Stat, <<"change">> := Change }) ->
    { translate_stat(Stat), Change }
  end, InCons).

translate_stat(<<"health">>) ->
  health;
translate_stat(<<"confidence">>) ->
  confidence;
translate_stat(<<"focus">>) ->
  focus.

stage_to_map(Stage) ->
  #{ <<"description">> => disc_stage:get_description(Stage),
     <<"id">> => uuid:uuid_to_string(disc_stage:get_id(Stage), binary_standard),
     <<"success">> => convert_outcome_to_map(disc_stage:get_success(Stage)),
     <<"failure">> => convert_outcome_to_map(disc_stage:get_failure(Stage))
  }.

convert_outcome_to_map(Outcome) ->
  Message = disc_stage:get_outcome_message(Outcome),
  Cons = lists:map(fun({Stat, Change}) ->
    #{<<"stat">> => Stat, <<"change">> => Change}
  end, disc_stage:get_outcome_consequences(Outcome)),
  #{ <<"message">> => Message, <<"consequences">> => Cons }.
