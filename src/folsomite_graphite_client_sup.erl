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

-module(folsomite_graphite_client_sup).
-behaviour(supervisor).

-export([get_client/0]).
-export([reconnect_client/0]).
-export([start_link/0]).
-export([init/1]).

-define(APP, folsomite).

%% api
get_client() ->
    case start_client(get_env(graphite_host), get_env(graphite_port)) of
        {error, {already_started, Pid}} -> {ok, Pid};
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

%% management api
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

reconnect_client() ->
    {ok, Pid} = get_client(),
    Pid ! connect,
    ok.

%% supervisor callback
init(no_arg) -> {ok, {{one_for_one, 5, 10}, []}}.


%% internal
start_client(Host, Port) ->
      supervisor:start_child(
        ?MODULE,
        {?APP,
         {folsomite_graphite_client, start_link, [Host, Port]},
         temporary,
         brutal_kill,
         worker,
         [folsomite_graphite_client]}).

get_env(Name) ->
    {ok, Value} = application:get_env(?APP, Name),
    Value.
