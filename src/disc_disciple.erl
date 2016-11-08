-module(disc_disciple).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(disciple, {
  skill = 100,
  confidence = 100,
  rand_state
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
  {ok, #disciple{rand_state=rand:seed(exsplus)}}.

handle_call({train, Difficulty}, _From, #disciple{skill=Skill}=Disciple) ->
  {Success, SkillChange, NextState} = calculate_training_result(Disciple, Difficulty),
  NewConfidence = adjust_confidence_on_result(Success, Disciple, Difficulty),
  io:format("Confidence now ~p~n", [NewConfidence]),
  {reply, {Success, SkillChange}, Disciple#disciple{confidence = NewConfidence, skill=Skill + SkillChange, rand_state=NextState}};
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

% Easy
adjust_confidence_on_result(success, #disciple{skill=Skill, confidence=Confidence}, Difficulty) when Difficulty < Skill / 2 ->
  Confidence + 0.1;
adjust_confidence_on_result(failure, #disciple{skill=Skill, confidence=Confidence}, Difficulty) when Difficulty < Skill / 2 ->
  Confidence - 1;
% Medium
adjust_confidence_on_result(success, #disciple{skill=Skill, confidence=Confidence}, Difficulty) when Difficulty < Skill * 0.8 ->
  Confidence + 0.3;
adjust_confidence_on_result(failure, #disciple{skill=Skill, confidence=Confidence}, Difficulty) when Difficulty < Skill * 0.8 ->
  Confidence - 0.5;
% Hard
adjust_confidence_on_result(success, #disciple{skill=Skill, confidence=Confidence}, Difficulty) when Difficulty < Skill * 1.2 ->
  Confidence + 0.5;
adjust_confidence_on_result(failure, #disciple{skill=Skill, confidence=Confidence}, Difficulty) when Difficulty < Skill * 1.2 ->
  Confidence - 0.3;
% Very Hard
adjust_confidence_on_result(success, #disciple{confidence=Confidence}, _Difficulty) ->
  Confidence + 1;
adjust_confidence_on_result(failure, #disciple{confidence=Confidence}, _Difficulty) ->
  Confidence - 0.1.

apply_caps(_Min, Max, Value) when Value > Max ->
  Max;
apply_caps(Min, _Max, Value) when Value < Min ->
  Min;
apply_caps(_Min, _Max, Value) ->
  Value.

apply_confidence(Skill, Confidence) when Confidence > 110 ->
  Skill - ((Confidence - 110) * (Confidence - 110)) / 4;
apply_confidence(Skill, Confidence) when Confidence < 90 ->
  Skill - ((90 - Confidence) * (90 - Confidence)) / 4;
apply_confidence(Skill, _Confidence) ->
  Skill.


calculate_training_result(Disciple, Difficulty) ->
  case attempt_challenge(Disciple, Difficulty) of
    {success, NextState} ->
      {success, Difficulty / 100, NextState};
    {failure, NextState} ->
      {failure, Difficulty / 1000, NextState}
  end.

attempt_challenge(#disciple{skill=Skill, rand_state=RandState, confidence=Confidence}, Difficulty) ->
  io:format("Attempting training with: Skill: ~p Difficulty:~p Confidence:~p~n", [Skill, Difficulty, Confidence]),
  {SkillRoll,NextState} = rand:normal_s(RandState),
  io:format("SKill Roll: ~p~n", [SkillRoll]),
  ConSkill = apply_confidence(Skill, Confidence),
  io:format("Confidence skill: ~p~n", [ConSkill]),
  SkillResult = apply_caps(0, 10 * Skill, (SkillRoll + 5) * ConSkill),
  io:format("Final Value: ~p~n", [SkillResult]),
  case SkillResult of
    SkillResult when SkillResult >  5 * Difficulty ->
      io:format("Success!~n"),
      {success, NextState};
    SkillResult ->
      io:format("failure!~n"),
      {failure, NextState}
  end.
