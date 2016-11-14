-module(disc_stage).

-export([init_ets/0, get_stage/1, new_outcome/2, new/4, save/1, apply_stage/2, all/0]).
-export([get_id/1, get_description/1, get_difficulty/1, get_success/1, get_failure/1]).
-export([get_outcome_message/1, get_outcome_consequences/1]).
-export([update/5]).


-record(stage, {id, description, difficulty, success, failure}).
-record(outcome, {message, consequences=[]}).

init_ets() ->
  ets:new(disc_stage, [ordered_set, {keypos, #stage.id}, public, named_table]).

all() ->
  disc_ets_utils:all(disc_stage).

get_stage(StageId) ->
  [Stage] = ets:lookup(disc_stage, StageId),
  Stage.

new_outcome(Message, Cons) ->
  #outcome{message=Message, consequences=Cons}.

get_outcome_message(#outcome{message=Message}) ->
  Message.
get_outcome_consequences(#outcome{consequences=Consequences}) ->
  Consequences.

get_id(#stage{id=Id}) ->
  Id.
get_description(#stage{description=Description}) ->
  Description.
get_difficulty(#stage{difficulty=Difficulty}) ->
  Difficulty.
get_failure(#stage{failure=Failure}) ->
  Failure.
get_success(#stage{success=Success}) ->
  Success.

new(Description, Difficulty, SuccessOutcome, FailureOutcome) ->
  #stage{id=uuid:get_v4(),
         description=Description,
	 difficulty=Difficulty,
	 success=SuccessOutcome,
	 failure=FailureOutcome}.

update(Id, Description, Difficulty, SuccessOutcome, FailureOutcome) ->
  #stage{id=Id,
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
