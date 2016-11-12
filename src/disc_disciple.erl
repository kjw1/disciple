-module(disc_disciple).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([face_challenge/4]).
-export([debug_print/1]).
-export([get_id/1]).
-export([get_stats/1]).
-export([give_feedback/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(disciple, {
  id,
  name = <<"Sara">>,
  skill = 100,
  health = 100,
  confidence = 100,
  pride = 100,
  focus = 100,
  discipline = 100,
  rand_state
}).

%% API.

-spec start_link(binary()) -> {ok, pid()}.
start_link(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

face_challenge(Disciple, Challenge, Success, Failure) ->
  gen_server:call(Disciple, {challenge, Challenge, Success, Failure}).

debug_print(Disciple) ->
  gen_server:cast(Disciple, debug_print).

get_stats(Disciple) ->
  gen_server:call(Disciple, get_stats).

get_id(Disciple) ->
  gen_server:call(Disciple, get_id).

give_feedback(Disciple, encourage) ->
  gen_server:call(Disciple, {feedback, encourage});
give_feedback(Disciple, chastise) ->
  gen_server:call(Disciple, {feedback, chastise});
give_feedback(Disciple, refocus) ->
  gen_server:call(Disciple, {feedback, refocus}).

%% gen_server.

init([Name]) ->
  Id = uuid:get_v4(),
  disc_disciple_locator:register_disciple(Id, self()),
  RandState = rand:seed(exsplus),
  {SkillRoll,RandState2} = rand:normal_s(RandState),
  {HealthRoll,RandState3} = rand:normal_s(RandState2),
  {ConfidenceRoll,RandState4} = rand:normal_s(RandState3),
  {PrideRoll,RandState5} = rand:normal_s(RandState4),
  {FocusRoll,RandState6} = rand:normal_s(RandState5),
  {DisciplineRoll,RandState7} = rand:normal_s(RandState6),
  Skill = apply_caps(50, 150, 100 + SkillRoll * 10),
  Health = apply_caps(30, 180, 100 + HealthRoll * 20),
  Confidence = apply_caps(30, 180, 100 + ConfidenceRoll * 20),
  Pride = apply_caps(30, 180, 100 + PrideRoll * 30),
  Focus = apply_caps(0, 100, 80 + FocusRoll * 20),
  Discipline = apply_caps(20, 200, 100 + DisciplineRoll * 30),
  {ok, #disciple{
    id=Id, name=Name,
    skill=Skill,
    health=Health,
    confidence=Confidence,
    pride=Pride,
    focus=Focus,
    discipline=Discipline,
    rand_state=RandState7}}.

handle_call({feedback, refocus}, _From, #disciple{discipline=Discipline, focus=Focus}=Disciple) ->
  NewFocus = apply_caps(0, 100, Focus + refocus_focus_change(Discipline)),
  io:format("Focus now ~p~n", [NewFocus]),
  {reply, NewFocus, Disciple#disciple{focus=NewFocus}};
handle_call({feedback, encourage}, _From, #disciple{pride=Pride, confidence=Confidence}=Disciple) ->
  NewConfidence = apply_caps(0, 200, Confidence + encourage_confidence_change(Pride)),
  io:format("Confidence now ~p~n", [NewConfidence]),
  {reply, NewConfidence, Disciple#disciple{confidence=NewConfidence}};
handle_call({feedback, chastise}, _From, #disciple{pride=Pride, focus=Focus, confidence=Confidence}=Disciple) ->
  NewConfidence = apply_caps(0, 200, Confidence + chastise_confidence_change(Pride)),
  NewFocus = apply_caps(0, 100, Focus + 10),
  io:format("Confidence now ~p, focus now ~p~n", [NewConfidence, NewFocus]),
  {reply, {NewConfidence, NewFocus}, Disciple#disciple{focus=NewFocus, confidence=NewConfidence}};
handle_call({challenge, Difficulty, SuccessCons, FailureCons}, _From, #disciple{focus=Focus, discipline=Discipline, skill=Skill}=Disciple) ->
  {Success, SkillChange, NextState} = calculate_challenge_result(Disciple, Difficulty),
  NewConfidence = adjust_confidence_on_result(Success, Disciple, Difficulty),
  NewFocus = apply_caps(0, 100, Focus + adjust_focus_on_difficulty(Success, Skill, Discipline, Difficulty)),
  io:format("Confidence now ~p, Focus ~p~n", [NewConfidence, NewFocus]),
  NewDisciple = Disciple#disciple{confidence = NewConfidence, focus=NewFocus, skill=Skill + SkillChange, rand_state=NextState},
  {reply, Success, apply_cons(Success, NewDisciple, SuccessCons, FailureCons)};
handle_call(get_id, _From, #disciple{id=Id}=Disciple) ->
  {reply, Id, Disciple};
handle_call(get_stats, _Ref, #disciple{
    name=Name,
    health=Health,
    skill=Skill,
    confidence=Confidence,
    pride=Pride,
    discipline=Discipline,
    focus=Focus
  }=Disciple) ->
  Reply = #{ 
             name => Name,
             health => Health,
             skill => Skill,
             confidence => Confidence,
             pride => Pride,
             discipline => Discipline,
             focus => Focus
           },
  {reply, Reply, Disciple};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(debug_print, #disciple{
    id=Id,
    name=Name,
    health=Health,
    skill=Skill,
    confidence=Confidence,
    pride=Pride,
    discipline=Discipline,
    focus=Focus
  }=Disciple) ->
  io:format("Id ~p Name ~p Health ~p Skill ~p Confidence ~p Pride ~p Discipline ~p Focus ~p~n", [
    uuid:uuid_to_string(Id, binary_standard), Name, Health, Skill, Confidence, Pride, Discipline, Focus
  ]),
  {noreply, Disciple};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% helpers

apply_cons(success, Disciple, SuccessCons, _FailureCons) ->
  lists:foldl(fun apply_con/2, Disciple, SuccessCons);
apply_cons(failure, Disciple, _SuccessCons, FailureCons) ->
  lists:foldl(fun apply_con/2, Disciple, FailureCons).

apply_con({health, HealthChange}, #disciple{health=Health}=Disciple) ->
  NewHealth = apply_caps(0, 100, Health + HealthChange),
  Disciple#disciple{health=NewHealth};
apply_con({focus, FocusChange}, #disciple{focus=Focus}=Disciple) ->
  NewFocus = apply_caps(0, 100, Focus + FocusChange),
  Disciple#disciple{focus=NewFocus};
apply_con({confidence, ConfidenceChange}, #disciple{confidence=Confidence}=Disciple) ->
  NewConfidence = apply_caps(0, 200, Confidence + ConfidenceChange),
  Disciple#disciple{confidence=NewConfidence}.

adjust_focus_on_difficulty(success, Skill, Discipline, Difficulty) ->
  case judge_difficulty(Skill, Difficulty) of
    easy -> -1 / (Discipline / 100);
    medium -> -0.5 / (Discipline / 100);
    _Other -> -0.1 / (Discipline / 100)
  end;
adjust_focus_on_difficulty(failure, Skill, Discipline, Difficulty) ->
  case judge_difficulty(Skill, Difficulty) of
    easy -> 1 * (Discipline / 100);
    medium -> 0.5 * (Discipline / 100);
    _Other -> 0.1 * (Discipline / 100)
  end.
encourage_confidence_change(Pride) ->
  5 * (Pride / 100).
chastise_confidence_change(Pride) ->
  -5 / (Pride / 100).
refocus_focus_change(Discipline) ->
  5 * (Discipline / 100).

judge_difficulty(Skill, Difficulty) when Difficulty < Skill / 2 ->
  easy;
judge_difficulty(Skill, Difficulty) when Difficulty < Skill * 0.8 ->
  medium;
judge_difficulty(Skill, Difficulty) when Difficulty < Skill * 1.2 ->
  hard;
judge_difficulty(_Skill, _Difficulty)->
  very_hard.

adjust_confidence_on_result(success, #disciple{skill=Skill, pride=Pride, confidence=Confidence}, Difficulty) ->
  case judge_difficulty(Skill, Difficulty) of
    easy -> Confidence + 0.1 * Pride / 100;
    medium -> Confidence + 0.3 * Pride / 100;
    hard -> Confidence + 0.5 * Pride / 100;
    very_hard -> Confidence + 1 * Pride / 100
  end;
adjust_confidence_on_result(failure, #disciple{skill=Skill, pride=Pride, confidence=Confidence}, Difficulty) ->
  case judge_difficulty(Skill, Difficulty) of
    easy -> Confidence - 1 / (Pride / 100);
    medium -> Confidence - 0.5 / (Pride / 100);
    hard -> Confidence - 0.3 / (Pride / 100);
    very_hard -> Confidence - 0.1 / (Pride / 100)
  end.

apply_caps(_Min, Max, Value) when Value > Max ->
  Max;
apply_caps(Min, _Max, Value) when Value < Min ->
  Min;
apply_caps(_Min, _Max, Value) ->
  Value.

apply_range_modifier(Skill, Stat) when Stat > 110 ->
  Skill - ((Stat - 110) * (Stat - 110)) / 4;
apply_range_modifier(Skill, Stat) when Stat < 90 ->
  Skill - ((90 - Stat) * (90 - Stat)) / 4;
apply_range_modifier(Skill, _Stat) ->
  Skill.


calculate_challenge_result(Disciple, Difficulty) ->
  case attempt_challenge(Disciple, Difficulty) of
    {success, NextState} ->
      {success, Difficulty / 100, NextState};
    {failure, NextState} ->
      {failure, Difficulty / 1000, NextState}
  end.

attempt_challenge(#disciple{skill=Skill, focus=Focus, rand_state=RandState, confidence=Confidence}, Difficulty) ->
  io:format("Attempting challenge with: Skill: ~p Difficulty:~p Confidence:~p~n", [Skill, Difficulty, Confidence]),
  {SkillRoll,NextState} = rand:normal_s(RandState),
  io:format("SKill Roll: ~p~n", [SkillRoll]),
  ConSkill = apply_range_modifier(Skill, Confidence),
  ConFocusSkill = apply_range_modifier(ConSkill, Focus),
  io:format("Confidence + Focus skill: ~p~n", [ConFocusSkill]),
  SkillResult = apply_caps(0, 10 * Skill, (SkillRoll + 5) * ConFocusSkill),
  io:format("Final Value: ~p~n", [SkillResult]),
  case SkillResult of
    SkillResult when SkillResult >  5 * Difficulty ->
      io:format("Success!~n"),
      {success, NextState};
    SkillResult ->
      io:format("failure!~n"),
      {failure, NextState}
  end.
