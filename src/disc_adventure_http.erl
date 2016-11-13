-module(disc_adventure_http).
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
  #{ <<"name">> := Name, <<"stages">> := Stages } = jiffy:decode(Body, [return_maps]),
  Ad1 = disc_adventure:new(Name),
  Ad2 = add_all_stages(Stages, Ad1),
  AdReply = adventure_to_map(Ad2),
  {ok, Req3} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>}],
    jiffy:encode(AdReply), Req2),
  Req3.


adventure_to_map(Ad) ->
  #{ <<"name">> => disc_adventure:get_name(Ad),
     <<"id">> => uuid:uuid_to_string(disc_adventure:get_id(Ad), binary_standard),
     <<"stages">> => lists:foldl(fun(StageId, Stages) -> 
       [ uuid:uuid_to_string(StageId, binary_standard) | Stages ]
     end, [], disc_adventure:get_stages(Ad))
  }.

add_all_stages(Stages, Ad1) ->
  lists:foldl(fun(StageId, Ad) ->
    StageUuid = uuid:string_to_uuid(StageId),
    disc_adventure:add_stage(Ad, StageUuid)
  end, Ad1, Stages).
