-module(proto_stat).
-behaviour(gen_server).

-export([addr/2]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("ncp.hrl").

%
% notify all clients
%

addr(Op, IP) -> 
    gen_server:cast(?MODULE, {addr, Op, IP}).

%
% genserver
%

start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    S = dict:new(),
    os:cmd(?IPTABLES_FLUSH),
    S1 = add(?LOCALNET, S),
    ?AFTER(0, check),
    {ok, S1}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast({addr, add, IP}, State) -> 
    NewState = add(IP, State),
    {noreply, NewState};

handle_cast({addr, del, IP}, State) -> 
    NewState = del(IP, State),
    {noreply, NewState};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(check, State) ->
    Reply = os:cmd(?IPTABLES_STAT),
    NState = extract(State, Reply),
    broadcaster:send({hosts, stat, to_list(NState)}),
    ?AFTER(?PROTO_PULL_PERIOD, check),
    {noreply, NState};

handle_info(_Info, State) ->  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% local
%

add(IP, S) ->
    iptables("A", IP),
    dict:store(IP, [], S).

del(IP, S) ->
    iptables("D", IP),
    dict:erase(IP, S).

iptables(Op, IP) ->
    Cmds = lists:foldl(fun(I, A) ->
            In = io_lib:format(?IPTABLES_SRC, [IP, Op, I]),
            Out = io_lib:format(?IPTABLES_DST, [IP, Op, I]),
            [In, Out | A]
        end, [], ?PROTOCOLS),
    [os:cmd(C) || C <- Cmds].

extract(S, UnpReply) ->
    %%        9     3791 ACCEPT     all  --  *      *       0.0.0.0/0            0.0.0.0/0           protocol HTTP 
    {ok, RE} = re:compile("[ ]+([0-9]*)[ ]+([0-9]*)[ ]+ACCEPT[ ]+all[ ]+--[ ]+\\*[ ]+\\*[ ]+([0-9\\./]+)[ ]+([0-9\\./]+)[ ]+protocol ([0-9a-zA-Z ]+)\n"),
    case re:run(UnpReply, RE, [global, {capture, all, list}]) of
        {match, Reply} ->
            dict:fold(fun(K, _V, A) -> 
                    E = extract_item(K, Reply, []),
                    EF = [{string:strip(Pr, both), D, list_to_integer(P), list_to_integer(B)} || {Pr, D, P, B} <- E],
                    dict:store(K, EF, A) 
                end, dict:new(), S);
        _ ->
            S
    end.

extract_item(_IP, [], R) ->
    R;

extract_item(IP, [[_, Pkts, Bytes, Src, _Dst, Proto]|Reply], R) when (Src =:= IP) and (Pkts =/= "0") ->
    extract_item(IP, Reply, R ++ [{Proto, out, Pkts, Bytes}]);

extract_item(IP, [[_, Pkts, Bytes, _Src, Dst, Proto]|Reply], R) when (Dst =:= IP) and (Pkts =/= "0") ->
    extract_item(IP, Reply, R ++ [{Proto, in, Pkts, Bytes}]);

extract_item(IP, [_Any|Reply], R) ->
    extract_item(IP, Reply, R).


to_list(S) ->
    dict:fold(fun(K, V, A) -> A ++ [{K, V}] end, [], S).

