-module(disc_test_run).

-export([do_run/0]).
-export([test_adventure/0]).

do_run() ->
  {ok, D} = disc_disciple:start_link(),
  disc_disciple:face_challenge(D, 100),
  disc_disciple:face_challenge(D, 100),
  disc_disciple:face_challenge(D, 100).


test_adventure() ->
  Ad1 = disc_adventure:new(),
  Stages = [ {"find some kobolds", 100,
               {"kobold camp found", []}, {"didn't find anything", [{focus, -1}]}
             },
             {"fight the kobolds", 100,
               {"kobolds killed", [{confidence,2}]},
               {"kobolds force a retreat", [{health, -10}]}
             } ],
  Ad2 = lists:foldl(fun add_stages/2, Ad1, Stages),
  {ok, D} = disc_disciple:start_link(),
  disc_adventure:go(Ad2, D).


add_stages({Description, Difficulty, Success, Failure}, Ad) ->
  disc_adventure:add_stage(Ad, Description, Difficulty, Success, Failure).
  
