%% -----------------------------------------------------------------------------
%%
%% fastlog_server: TBD
%%
%% Copyright (c) 2008-2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%%
%% TBC
%%
%% -----------------------------------------------------------------------------

-module(fastlog_sup).
-behaviour(supervisor).
-include("fastlog.hrl").

%% API
-export([start_link/0, start_link/1, 
        add_logger/1, add_logger/2, remove_logger/1]).

%% Supervisor callbacks
-export([init/1]).

-type(start_result() :: {'ok', pid()} | 'ignore' | {'error', any()}).

%% -----------------------------------------------------------------------------
%% API functions
%% -----------------------------------------------------------------------------

-spec(start_link/0 :: () -> start_result()).
start_link() ->
    Options = case application:get_all_env(fastlog) of
        [] -> [{level, info}, {name, fastlog}];
        Other -> Other
    end,
    start_link(Options).

-spec(start_link/1 :: ([{atom(), term()}]) -> start_result()).
start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Options).

add_logger(Name) when is_atom(Name) orelse is_list(Name) ->
    add_logger(Name, [[{level, info}]]).
    
add_logger(Name, Config) when is_atom(Name) ->
    add_logger(atom_to_list(Name), Config);
add_logger(Name, Config) when is_list(Name) ->
    Server = fastlog:server_name(Name),
    case lists:keyfind(Server, 1, supervisor:which_children(?MODULE)) of
        false ->
            ChildSpec = {Server,
                        {fastlog_server, start_link, [[{name, Server}|Config]]},
                         permanent, brutal_kill, worker, [gen_server]},
            supervisor:start_child(?MODULE, ChildSpec);
        _ ->
            supervisor:restart_child(?MODULE, Server)
    end.

remove_logger(Name) ->
    supervisor:terminate_child(?MODULE, Name).

%% -----------------------------------------------------------------------------
%% Supervisor callbacks
%% -----------------------------------------------------------------------------

init([]) ->
    init(application:get_all_env(fastlog));
init(Args) ->
    {ok, {{one_for_one, 5, 10}, [
        {fastlog, %% this is the top level logger
            {fastlog_server, start_link, [Args]},
            permanent, 5000, worker, [gen_server]}
    ]}}.

