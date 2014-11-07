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

-module(folsomite_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Application callbacks
start(normal, no_arg) ->
    folsom_metrics:new_histogram(
      {folsomite, send_stats}, slide_uniform, {60, 1028}),
    folsomite_sup:start_link().

stop(_) -> ok.
