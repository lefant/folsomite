%% Copyright (c) 2012 Campanja AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% A copy of the license is included in the file LICENSE.
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(folsomite_graphite_client).
-behaviour(gen_server).

-export([start_link/2]).
-export([send/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3]).

-record(state, {tcp_socket :: inet:socket()}).

%% management api
start_link(Host, Port) -> gen_server:start_link(?MODULE, [Host, Port], []).

%% api
send(Socket, Message) -> gen_server:cast(Socket, {send, Message}).

%% gen_server callbacks
init([Host, Port]) ->
    {ok, TCPSocket} = gen_tcp:connect(Host,
                                      Port,
                                      [binary, {active, false}],
                                      5000),
    {ok, #state{tcp_socket = TCPSocket}}.

handle_call(Call, _, State) ->
    unexpected(call, Call),
    {noreply, State}.

handle_cast({send, Message}, #state{tcp_socket = TCPSocket} = State) ->
    ok = gen_tcp:send(TCPSocket, Message),
    {noreply, State};
handle_cast(Cast, State) ->
    unexpected(cast, Cast),
    {noreply, State}.

handle_info(Info, State) ->
    unexpected(info, Info),
    {noreply, State}.

terminate(_, #state{tcp_socket = TCPSocket}) ->
    case TCPSocket of
        undefined -> ok;
        TCPSocket1 -> gen_tcp:close(TCPSocket1)
    end.

code_change(_, State, _) -> {ok, State}.


%% internal
unexpected(Type, Message) ->
    error_logger:info_msg(" unexpected ~p ~p~n", [Type, Message]).
