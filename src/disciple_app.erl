-module(disciple_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [ {"/1/disciple/[:id/[:action]]", disc_disciple_http, []},
            {"/1/adventure/[:id]", disc_adventure_http, []},
            {"/1/stage/[:id]", disc_stage_http, []},
            {"/", cowboy_static, {priv_file, disciple, "static/index.html"}},
            {"/[...]", cowboy_static, {priv_dir, disciple, "static/"}}
          ]
    }
  ]),
  cowboy:start_http(disc_disciple_cowboy_http, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  disc_disciple_locator:init_ets(),
  disc_adventure:init_ets(),
  disc_stage:init_ets(),
  disciple_sup:start_link().

stop(_State) ->
  ok.
