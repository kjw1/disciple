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
  rand_state
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
  {ok, #disciple{rand_state=rand:seed(exsplus)}}.

handle_call({train, Difficulty}, _From, #disciple{skill=Skill,rand_state=RandState}=Disciple) ->
  {{Success, SkillChange}, NextState} = calculate_training_result(Skill, Difficulty, RandState),
  {reply, {Success, SkillChange}, Disciple#disciple{skill=Skill + SkillChange, rand_state=NextState}};
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

apply_caps(_Min, Max, Value) when Value > Max ->
  Max;
apply_caps(Min, _Max, Value) when Value < Min ->
  Min;
apply_caps(_Min, _Max, Value) ->
  Value.

calculate_training_result(Skill, Difficulty, RandState) ->
  io:format("Attempting training with: Skill: ~p Difficulty:~p~n", [Skill, Difficulty]),
  {SkillRoll,NextState} = rand:normal_s(RandState),
  io:format("SKill Roll: ~p~n", [SkillRoll]),
  SkillResult = apply_caps(0, 10 * Skill, (SkillRoll + 5) * Skill),
  io:format("Final Value: ~p~n", [SkillResult]),
  Success = case SkillResult of
    SkillResult when SkillResult > Difficulty ->
      io:format("Success!~n"),
      {success, Difficulty / 100};
    SkillResult ->
      io:format("failure!~n"),
      {failure, 0}
  end,
  {Success, NextState}.
