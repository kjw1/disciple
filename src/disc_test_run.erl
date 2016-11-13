-module(disc_test_run).

-export([do_run/0]).
-export([test_adventure/0]).

do_run() ->
  {ok, D} = disc_disciple:start_link(<<"Sara">>),
  disc_disciple:face_challenge(D, 100),
  disc_disciple:face_challenge(D, 100),
  disc_disciple:face_challenge(D, 100).


test_adventure() ->
  Ad1 = disc_adventure:new(),
  Stages = [ {"find some kobolds", 100,
               {"kobold camp found", []}, {"didn't find anything", [{focus, -1}]}
             },
             {"fight the kobolds", 100,
               {"kobolds killed", [{confidence,2}, {skill,2}]},
               {"kobolds force a retreat", [{health, -10}]}
             } ],
  Ad2 = lists:foldl(fun add_stages/2, Ad1, Stages),
  {ok, D} = disc_disciple:start_link(<<"Sara">>),
  disc_adventure:go(Ad2, D).


add_stages({Description, Difficulty, {SMessage, SCons}, {FMessage, FCons}}, Ad) ->
  SuccessOutcome = disc_stage:new_outcome(SMessage, SCons),
  FailureOutcome = disc_stage:new_outcome(FMessage, FCons),
  Stage = disc_stage:new(Description, Difficulty, SuccessOutcome, FailureOutcome),
  disc_stage:save(Stage),
  StageId = disc_stage:get_id(Stage),
  disc_adventure:add_stage(Ad, StageId).

  
