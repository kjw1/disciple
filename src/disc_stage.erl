-module(disc_stage).

-export([init_ets/0, get_stage/1, new_outcome/2, new/4, save/1, apply_stage/2]).
-export([get_id/1]).

-record(stage, {id, description, difficulty, success, failure}).
-record(outcome, {message, consequences=[]}).

init_ets() ->
  ets:new(disc_stage, [ordered_set, {keypos, #stage.id}, public, named_table]).

get_stage(StageId) ->
  [Stage] = ets:lookup(disc_stage, StageId),
  Stage.

new_outcome(Message, Cons) ->
  #outcome{message=Message, consequences=Cons}.

get_id(#stage{id=Id}) ->
  Id.

new(Description, Difficulty, SuccessOutcome, FailureOutcome) ->
  #stage{id=uuid:get_v4(),
         description=Description,
	 difficulty=Difficulty,
	 success=SuccessOutcome,
	 failure=FailureOutcome}.

save(#stage{}=Stage) ->
  ets:insert(disc_stage, Stage).

apply_stage(Disciple,
            #stage{description=Description,
                   difficulty=Difficulty,
                   success=#outcome{message=SuccessMessage, consequences=SuccessCons},
                   failure=#outcome{message=FailureMessage, consequences=FailureCons}}) ->
  io:format("New stage: ~p~n", [Description]),
  disc_disciple:debug_print(Disciple),
  case disc_disciple:face_challenge(Disciple, Difficulty, SuccessCons, FailureCons) of
    success ->
      io:format("Success: ~p~n", [SuccessMessage]),
      disc_disciple:debug_print(Disciple),
      {success, SuccessMessage};
    failure ->
      io:format("Failure: ~p~n", [FailureMessage]),
      {failure, FailureMessage }
  end.
