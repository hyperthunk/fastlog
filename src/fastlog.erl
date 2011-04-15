%% -----------------------------------------------------------------------------
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
%%
%% -----------------------------------------------------------------------------
%%
%% @doc This module provides the high level fastlog API.
%%
%% -----------------------------------------------------------------------------

-module(fastlog).
-export([start/0
        ,stop/0
        ,configure/1
        ,add_logger/1
        ,add_logger/2
        ,remove_logger/1
        ,server_name/1
        ,set_level/1
        ,set_level/2
        ,get_level/0
        ,get_level/1
        ,debug/1
        ,debug/2
        ,debug/3
        ,info/1
        ,info/2
        ,info/3
        ,warn/1
        ,warn/2
        ,warn/3
        ,error/1
        ,error/2
        ,error/3]).

-type(level() :: debug | info | warn | error | off).

-spec(set_level/1   :: (level()) -> {ok, level()}).
-spec(get_level/0   :: () -> level()).
-spec(debug/1       :: (string()) -> 'ok').
-spec(debug/2       :: (string(), [any()]) -> 'ok').
-spec(info/1        :: (string()) -> 'ok').
-spec(info/2        :: (string(), [any()]) -> 'ok').
-spec(warn/1        :: (string()) -> 'ok').
-spec(warn/2        :: (string(), [any()]) -> 'ok').
-spec(error/1       :: (string()) -> 'ok').
-spec(error/2       :: (string(), [any()]) -> 'ok').

%% -----------------------------------------------------------------------------
%% Public API Functions
%% -----------------------------------------------------------------------------

%% @doc Starts the fastlog application
start() ->
    appstart:start(fastlog).

%% @doc Stops the fastlog application
stop() ->
    application:stop(fastlog).

%% @doc Adds a named logger.
add_logger(Name) ->
    fastlog_sup:add_logger(Name).

configure(AppName) ->
    case kvc:path(fastlog, application:get_all_env(AppName)) of
        [] ->
            ignored;
        Config ->
            [add_logger(Name, Conf) || {Name, Conf} <- Config]
    end.

%%
%% @doc Adds a named logger. If `Config' is an atom, then it
%% is set as the logging level, otherwise it is assumed to be
%% a proplist containing standard fastlog config and is passed on verbatim.
%%
add_logger(Name, Config) when is_atom(Config) ->
    add_logger(Name, [{level, Config}]);
add_logger(Name, Config) when is_list(Config) ->
    fastlog_sup:add_logger(Name, Config).

%% @ Removed the named logger.
remove_logger(Name) ->
    fastlog_sup:remove_logger(Name).

%% @doc Gets the current log level
get_level() ->
    get_level(fastlog_server).

get_level(Name) ->
    gen_server:call(server_name(Name), get_level).

%% @doc Sets the current log level
set_level(Lvl) ->
    set_level(fastlog_server, Lvl).

set_level(Name, Lvl) ->
    gen_server:call(server_name(Name), {set_level, Lvl}).

debug(Format) ->
    debug(fastlog_server, Format).

debug(Name, Format) when is_atom(Name) ->
    gen_server:cast(server_name(Name), {debug, Format});
debug(Format, Args) when is_list(Args) ->
    debug(fastlog_server, Format, Args).

debug(Name, Format, Args) when is_list(Args) ->
    gen_server:cast(server_name(Name), {debug, Format, Args}).

info(Format) ->
    info(fastlog_server, Format).

info(Name, Format) when is_atom(Name) ->
    gen_server:cast(server_name(Name), {info, Format});
info(Format, Args) when is_list(Args) ->
    info(fastlog_server, Format, Args).

info(Name, Format, Args) when is_list(Args) ->
    gen_server:cast(server_name(Name), {info, Format, Args}).

warn(Format) ->
    warn(fastlog_server, Format).
    
warn(Name, Format) when is_atom(Name) ->
    gen_server:cast(server_name(Name), {warn, Format});
warn(Format, Args) when is_list(Args) ->
    warn(fastlog_server, Format, Args).
    
warn(Name, Format, Args) when is_list(Args) ->
    gen_server:cast(server_name(Name), {warn, Format, Args}).

error(Format) ->
    fastlog:error(fastlog_server, Format).
    
error(Name, Format) when is_atom(Name) ->
    gen_server:cast(server_name(Name), {error, Format});
error(Format, Args) when is_list(Args) ->
    fastlog:error(fastlog_server, Format, Args).
    
error(Name, Format, Args) when is_list(Args) ->
    gen_server:cast(server_name(Name), {error, Format, Args}).

server_name(Name) when is_atom(Name) ->
    server_name(atom_to_list(Name));
server_name(Name) when is_list(Name) ->
    list_to_atom("fastlog." ++ Name).
