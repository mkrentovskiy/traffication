-module(ncp).

-export([start/0]).


start() ->
    ok = appstart(crypto),
    ok = appstart(xmerl),
    ok = appstart(sockjs),
    ok = appstart(ranch),
    ok = appstart(cowboy),
    ok = appstart(ncp).

appstart(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        Err -> error
    end.
