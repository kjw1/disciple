-module(disc_disciple).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([face_challenge/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(disciple, {
  skill = 100,
  health = 100,
  confidence = 100,
  pride = 100,
  focus = 100,
  discipline = 100,
  rand_state
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

face_challenge(Disciple, Challenge) ->
  gen_server:call(Disciple, {challenge, Challenge}).

%% gen_server.

init([]) ->
  {ok, #disciple{rand_state=rand:seed(exsplus)}}.

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
handle_call({challenge, Difficulty}, _From, #disciple{focus=Focus, discipline=Discipline, skill=Skill}=Disciple) ->
  {Success, SkillChange, NextState} = calculate_challenge_result(Disciple, Difficulty),
  NewConfidence = adjust_confidence_on_result(Success, Disciple, Difficulty),
  NewFocus = apply_caps(0, 100, Focus + adjust_focus_on_difficulty(Success, Skill, Discipline, Difficulty)),
  io:format("Confidence now ~p, Focus ~p~n", [NewConfidence, NewFocus]),
  NewDisciple = Disciple#disciple{confidence = NewConfidence, focus=NewFocus, skill=Skill + SkillChange, rand_state=NextState},
  {reply, Success, NewDisciple};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% helpers
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
  io:format("Attempting training with: Skill: ~p Difficulty:~p Confidence:~p~n", [Skill, Difficulty, Confidence]),
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
