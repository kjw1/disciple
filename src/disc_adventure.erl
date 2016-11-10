-module(disc_adventure).

-export([new/0, add_stage/5, go/2]).

-record(stage, {description, difficulty, success, failure}).
-record(adventure, {stages = []}).

new() ->
  #adventure{}.

add_stage(#adventure{stages=Stages}=Adventure, Description, Difficulty, SuccessMessage, FailureMessage) ->
  Adventure#adventure{stages = [
    #stage{description=Description,
           difficulty=Difficulty,
           success=SuccessMessage,
           failure=FailureMessage}
    | Stages]}.
  

go(#adventure{stages=Stages}, Disciple) ->
  apply_stages(Disciple, lists:reverse(Stages)).

apply_stages(_Disciple, []) ->
  success;
apply_stages(Disciple, [#stage{description=Description,
           difficulty=Difficulty,
           success={SuccessMessage, SuccessCons},
           failure={FailureMessage, FailureCons}} | Stages]) ->
  io:format("New stage: ~p~n", [Description]),
  disc_disciple:debug_print(Disciple),
  case disc_disciple:face_challenge(Disciple, Difficulty, SuccessCons, FailureCons) of
    success ->
      io:format("Success: ~p~n", [SuccessMessage]),
      disc_disciple:debug_print(Disciple),
      apply_stages(Disciple, Stages);
    failure ->
      io:format("Failure: ~p~n", [FailureMessage]),
      failure
  end.



