%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(test).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the test server.
start() ->
    test_deps:ensure(),
    ensure_started(crypto),
    application:start(test).

%% @spec stop() -> ok
%% @doc Stop the test server.
stop() ->
    Res = application:stop(test),
    application:stop(crypto),
    Res.
