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
  First = ets:first(disciple_location),
  collect_all(First, []).

collect_all('$end_of_table', Results) ->
  Results;
collect_all(Id, Results) ->
  Lookup = ets:lookup(disciple_location, Id),
  case Lookup of
    [#disciple_location{id=Id, pid=Pid}] ->
      try
        Stats = disc_disciple:get_stats(Pid),
        collect_all(ets:next(disciple_location, Id),
          [ Stats#{ id => uuid:uuid_to_string(Id, binary_standard) } | Results ])
      catch _Class:_Error ->
        collect_all(ets:next(disciple_location, Id), Results)
      end;
    [] ->
      collect_all(ets:next(disciple_location, Id), Results)
  end.
