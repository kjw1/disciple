-module(disc_disciple_locator).

-export([init_ets/0]).
-export([register_disciple/2]).
-export([find_disciple/1]).
-export([all/0]).

-record(disciple_location, {id, pid}).

init_ets() ->
  ets:new(disciple_location, [
    ordered_set,
    named_table,
    {keypos, #disciple_location.id},
    public
  ]).

register_disciple(Id, Disciple) ->
  ets:insert(disciple_location, #disciple_location{id=Id, pid=Disciple}).

find_disciple(Id) ->
  [#disciple_location{pid=Pid}] = ets:lookup(disciple_location, Id),
  Pid.

all() ->
  AllLocations = disc_ets_utils:all(disciple_location),
  lists:foldl(fun collect_stats/2, [], AllLocations).

collect_stats(#disciple_location{id=Id, pid=Pid}, Results) ->
  try
    Stats = disc_disciple:get_stats(Pid),
    [ Stats#{ id => uuid:uuid_to_string(Id, binary_standard) } | Results ]
  catch _Class:_Error ->
    Results
  end.
