-module(disciple_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [ {"/1/disciple/[:id/[:action]]", disc_disciple_http, []},
            {"/", cowboy_static, {priv_file, disciple, "static/index.html"}},
            {"/[...]", cowboy_static, {priv_dir, disciple, "static/"}}
          ]
    }
  ]),
  cowboy:start_http(disc_disciple_cowboy_http, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  disc_disciple_locator:init_ets(),
  disciple_sup:start_link().

stop(_State) ->
  ok.
