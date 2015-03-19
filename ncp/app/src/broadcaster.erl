-module(broadcaster).
-behaviour(gen_server).

-export([events/3, send/1]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%
% SockJS
%

events(Con, init, _) -> 
    gen_server:call(?MODULE, {add, Con}),
    {ok, undefined};

events(_Con, {recv, _Msg}, State) ->
    {ok, State};

events(Con, closed, State) ->
    gen_server:call(?MODULE, {remove, Con}),
    {ok, State}.

%
% notify all clients
%

send(Msg) -> 
	gen_server:call(?MODULE, {msg, Msg}).

%
% genserver
%

start() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {MS, S, _MkS} = os:timestamp(),
	{ok, {MS * 1000000 + S, []}}.

handle_call({add, Con}, _From, {StartTime, Cons}) ->
	History = data_source:history(),
	Con:send(encode({time, start, StartTime})),
    Con:send(encode(History)),
	{reply, ok, {StartTime, Cons ++ [Con]}};

handle_call({remove, Con}, _From, {StartTime, Cons}) -> 
	{reply, ok, {StartTime, lists:filter(fun(I) -> I =/= Con end, Cons)}};

handle_call({msg, Msg}, _From, State = {_StartTime, Cons}) ->
	[C:send(encode(Msg)) || C <- Cons],
	{reply, ok, State};

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) ->  {noreply, State}.

terminate(_Reason, {_StartTime, Cons}) -> 
	[C:close() || C <- Cons],
	ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% local
%

encode(Msg) ->
	base64:encode(bert:encode(Msg)).
