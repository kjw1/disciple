-module(disc_adventure).

-export([init_ets/0, new/0, add_stage/2, go/2, get_adventure/1, save/1]).
-export([get_id/1]).

-record(adventure, {id, stages = []}).

init_ets() ->
  ets:new(disc_adventure, [ordered_set, {keypos, #adventure.id}, public, named_table]).

get_adventure(Id) ->
  [Adventure] = ets:lookup(disc_adventure, Id),
  Adventure.

get_id(#adventure{id=Id}) ->
  Id.

save(#adventure{}=Adventure) ->
  ets:insert(disc_adventure, Adventure).

new() ->
  #adventure{id=uuid:get_v4()}.

add_stage(#adventure{stages=Stages}=Adventure, StageId) ->
  Adventure#adventure{stages = [ StageId | Stages ]}.

go(#adventure{stages=StageIds}, Disciple) ->
  Stages = lists:foldl(fun(StageId, StageAcc) ->
    [ disc_stage:get_stage(StageId) | StageAcc ]
  end, [], StageIds),
  apply_stages(Disciple, Stages, []).

apply_stages(_Disciple, [], Results) ->
  [ {success, "Adventure completed!"} | Results ];
apply_stages(Disciple, [Stage | Stages], Results) ->
  Result = disc_stage:apply_stage(Disciple, Stage),
  case Result of
    {success, _Message} ->
      apply_stages(Disciple, Stages, [Result | Results]);
    {failure, _Message} ->
      [ Result | Results ]
  end.



