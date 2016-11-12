-module(disc_disciple_locator).

-export([init_ets/0]).
-export([register_disciple/2]).
-export([find_disciple/1]).

-record(disciple_location, {id, pid}).

init_ets() ->
  ets:new(disciple_location, [named_table, {keypos, #disciple_location.id}, public]).

register_disciple(Id, Disciple) ->
  ets:insert(disciple_location, #disciple_location{id=Id, pid=Disciple}).

find_disciple(Id) ->
  [#disciple_location{pid=Pid}] = ets:lookup(disciple_location, Id),
  Pid.

