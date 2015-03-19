-module(ncp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    B = {broadcaster, {broadcaster, start, []}, permanent, 10000, worker, dynamic},
    DS = {data_source, {data_source, start, []}, permanent, 10000, worker, dynamic},
    PS = {proto_stat, {proto_stat, start, []}, permanent, 10000, worker, dynamic},
    {ok, { {one_for_one, 5, 10}, [B, DS, PS]} }.

