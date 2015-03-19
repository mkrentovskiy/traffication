-module(data_source).
-behaviour(gen_server).

-export([history/0]).
-export([dhcp/2]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("ncp.hrl").


history() -> 
	gen_server:call({global, ?MODULE}, history).

dhcp(Op, HostInfo) -> 
	gen_server:call({global, ?MODULE}, {dhcp, Op, HostInfo}).

%
% genserver
%

start() -> 
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
	case file:read_file("/tmp/dhcp.leases") of 
		{ok, File} ->
			Before = [binary:split(I, <<" ">>, [global]) || I <- binary:split(File, <<"\n">>, [global])],
			HostInfos = [
					[
						{"ip", binary_to_list(IPB)}, 
						{"mac", binary_to_list(MACB)}, 
						{"hostname", binary_to_list(HNB)} 
					] || [_, MACB, IPB, HNB, _] <- Before],
			State = lists:foldl(fun(I, A) -> add(A, I) end, dict:new(), HostInfos),
			{ok, State};	
		_ ->	
			{ok, dict:new()}
	end.

handle_call(history, _From, State) ->
	KVList = dict:fold(fun(K, V, A) -> A ++ [{K, V}] end, [], State), 
	{reply, {hosts, show, KVList}, State};

handle_call({dhcp, add, HostInfo}, _From, State) ->
	{reply, ok, add(State, HostInfo)};

handle_call({dhcp, del, HostInfo}, _From, State) ->
	{reply, ok, del(State, HostInfo)};

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({check, Key}, State) ->
	case dict:find(Key, State) of
		{ok, OldHostInfo} -> 
			?AFTER(?PULL_PERIOD, {check, Key}),
			case update_host_info(OldHostInfo) of
				OldHostInfo ->
					{noreply, State};	
				HostInfo ->
					FState = dict:store(Key, HostInfo, State),
					broadcaster:send({host, update, Key, HostInfo}),
					{noreply, FState}
			end;
		Any ->
			io:format("Found ~p~n", [Any]),
			{noreply, State}
	end;

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% local
%

add(State, HostInfo) ->
	Key = key(HostInfo),
	FState = dict:store(Key, HostInfo, State),
	broadcaster:send({host, show, Key, HostInfo}),
	% proto_stat:addr(add, proplists:get_value("ip", HostInfo)),
	?AFTER(0, {check, Key}),
	FState.

del(State, HostInfo) ->
	Key = key(HostInfo),
	FState = dict:erase(Key, State),
	broadcaster:send({host, hide, Key, HostInfo}),
	% proto_stat:addr(del, proplists:get_value("ip", HostInfo)),
	FState.

key(HostInfo) ->
	re:replace(proplists:get_value("ip", HostInfo), "\\.", "_", [global, {return, list}]) 
	++ "-" 
	++ re:replace(proplists:get_value("mac", HostInfo), ":", "_", [global, {return, list}]).

update_host_info(OldHostInfo) ->
	Cmd = ?P0F_COMMAND ++ proplists:get_value("ip", OldHostInfo),
	Reply = os:cmd(Cmd),
	case re:run(Reply, "([a-zA-Z ]*)[ ].= (.*)\n", [global, {capture, all, list}]) of
		{match, PList} ->
			[{A, proplists:get_value(A, OldHostInfo)} || A <- ["ip", "mac", "hostname"]] 
			++ 
			[{string:strip(K, both), V} || [_, K, V] <- PList];
		_ ->
			OldHostInfo
	end.
