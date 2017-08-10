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

-record(state, {socket :: inet:socket(),
                host :: inet:hostname(),
                port :: inet:port_number(),
                reconnect_time = 1000 :: non_neg_integer()
               }).

-define(MAX_RECONNECT_TIME, 30000).
-define(CONNECT_TIMEOUT, 5000).
-define(SEND_TIMEOUT, 30000).

start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

send(Socket, Message) ->
    gen_server:cast(Socket, {send, Message}).

init([Host, Port]) ->
    self() ! connect,
    {ok, #state{host=Host, port=Port}}.

handle_call(Call, _, State) ->
    unexpected(call, Call),
    {noreply, State}.

handle_cast({send, _Message}, #state{socket=undefined} = State) ->
    {noreply, State, hibernate};
handle_cast({send, Message}, #state{socket=Sock, reconnect_time=RCT} = State) ->
    case gen_tcp:send(Sock, Message) of
        ok ->
            {noreply, State, hibernate};
        {error, Reason} when Reason == econnrefused orelse
                             Reason == closed orelse
                             Reason == timeout ->
            ok = maybe_close_socket(Sock),
            error_logger:info_msg("Folsomite connection: ~s", [Reason]),
            erlang:send_after(RCT, self(), connect),
            {noreply, State#state{socket=undefined}}
    end;
handle_cast(Cast, State) ->
    unexpected(cast, Cast),
    {noreply, State}.

handle_info(connect, #state{host=Host, port=Port, reconnect_time=RCT, socket=OldSock} = State) ->
    error_logger:info_msg("Folsomite attempting to reconnect: ~s:~p", [Host, Port]),
    Opts = [binary, {active, false}, {send_timeout, ?SEND_TIMEOUT}],
    case gen_tcp:connect(Host, Port, Opts, ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            ok = gen_tcp:close(OldSock),
            {noreply, State#state{socket=Sock}};
        {error, Reason}  ->
            error_logger:info_msg("Folsomite connect failed: ~p", [Reason]),
            erlang:send_after(RCT, self(), connect),
            {noreply, State#state{reconnect_time=increase_connection_timeout(RCT)}}
    end;
handle_info(Info, State) ->
    unexpected(info, Info),
    {noreply, State}.

terminate(_, #state{socket=Sock}) ->
    ok = maybe_close_socket(Sock).

code_change(_, State, _) ->
    {ok, State}.

unexpected(Type, Message) ->
    error_logger:error_msg("Folsomite unexpected ~p ~p~n", [Type, Message]).

maybe_close_socket(undefined) ->
    ok;
maybe_close_socket(Sock) when is_port(Sock) ->
    ok = gen_tcp:close(Sock).

increase_connection_timeout(Current) ->
    min(Current*2, ?MAX_RECONNECT_TIME).
