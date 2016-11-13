-module(disc_ets_utils).
-export([all/1]).

%Only works on ordered_set tables!
all(Table) ->
  First = ets:first(Table),
  collect_all(Table, First, []).

collect_all(_Table, '$end_of_table', Results) ->
  Results;
collect_all(Table, Id, Results) ->
  Lookup = ets:lookup(Table, Id),
  case Lookup of
    [Item] ->
      collect_all(Table, ets:next(Table, Id), [ Item | Results ]);
    [] ->
      collect_all(Table, ets:next(Table, Id), Results)
  end.
