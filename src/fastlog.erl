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
-compile({no_auto_import,[error/2]}).
-include("fastlog.hrl").

-export([start/0, stop/0]).

-export([configure/1, load_config/1, add_logger/1,
         add_logger/2, remove_logger/1, server_name/1]).

-export([set_level/1, set_level/2,
         get_level/0, get_level/1, check_level/1]).

-export([debug/1, debug/2, debug/3]).
-export([info/1, info/2, info/3]).
-export([warn/1, warn/2, warn/3]).
-export([error/1, error/2, error/3]).

-export([sync_debug/1, sync_debug/2, sync_debug/3]).
-export([sync_info/1, sync_info/2, sync_info/3]).
-export([sync_warn/1, sync_warn/2, sync_warn/3]).
-export([sync_error/1, sync_error/2, sync_error/3]).

%% -----------------------------------------------------------------------------
%% Public API Functions
%% -----------------------------------------------------------------------------

%% @doc Starts the fastlog application
-spec(start/0 :: () -> 'ok' | term()).
start() ->
    appstart:start(fastlog).

%% @doc Stops the fastlog application
-spec(stop/0 :: () -> term()).
stop() ->
    application:stop(fastlog).

%% @doc Adds a named logger.
add_logger(Name) ->
    fastlog_sup:add_logger(Name).

%% @doc Configures the application defined by `AppName'. Relies on
%% application:get_all_env/1 to get back the 'fastlog' configuration.
%% @end
-spec(configure/1 :: (atom()) -> ignored | [logger_spec()]).
configure(AppName) when is_atom(AppName) ->
    load_config(application:get_all_env(AppName)).

-spec(load_config/1 :: (list(term())) -> ignored | [logger_spec()]).
load_config(AppData) ->
    case kvc:path(fastlog, AppData) of
        [] ->
            ignored;
        Config ->
            [add_logger(Name, Conf) || {Name, Conf} <- Config]
    end.

%%
%% @doc Adds a named logger. If `Config' is an atom, then it
%% is set as the logging level, otherwise it is assumed to be
%% a proplist containing fastlog config and is passed on verbatim.
%% @end
add_logger(Name, Config) when is_atom(Config) ->
    add_logger(Name, [{level, Config}]);
add_logger(Name, Config) when is_list(Config) ->
    fastlog_sup:add_logger(Name, Config).

%% @doc Removed the named logger.
-spec(remove_logger/1 :: (atom()) -> term()).
remove_logger(Name) ->
    fastlog_sup:remove_logger(Name).

%% @doc Gets the current log level for the top level logger
-spec(get_level/0 :: () -> level()).
get_level() ->
    get_level(fastlog).

%% @doc Gets the current log level for logger identified by `Name'
-spec(get_level/1 :: (atom()) -> level()).
get_level(Name) ->
    gen_server:call(server_name(Name), get_level).

-spec(check_level/1 :: (atom()) -> [{atom(), level()}]).
check_level(Name) ->
    %% TODO: reconsider where this belongs
    Loggers = supervisor:which_children(fastlog_sup),
    case lists:filter(match_appender(Name), Loggers) of
        [] ->
            %% we don't fall over if the logger isn't there...
            {none, Loggers};
        Items ->
            lists:zip(Items, lists:map(fun get_level/1, Items))
    end.

%% @doc Sets the current log level for the top level logger
-spec(set_level/1 :: (level()) -> {ok, level()}).
set_level(Lvl) ->
    set_level(fastlog, Lvl).

%% @doc Sets the current log level for the logger identified by `Name'
-spec(set_level/2 :: (atom(), level()) -> {ok, level()}).
set_level(Name, Lvl) ->
    gen_server:call(server_name(Name), {set_level, Lvl}).

log(Name, Data, Mode) ->
    %% TODO: reconsider where this belongs
    Loggers = supervisor:which_children(fastlog_sup),
    case lists:filter(match_appender(Name), Loggers) of
        [] ->
            %% we don't fall over if the logger isn't there...
            ok;
        Items ->
            [send_log_entry(S, Data, Mode) || {S,_,_,_} <- Items]
    end.

send_log_entry(S, Data, async) ->
    gen_server:cast(S, Data);
send_log_entry(S, Data, sync) ->
    gen_server:call(S, Data).

match_appender(Name) ->
    fun({Pattern,_,_,_}) ->
        match_appender(atom_to_binary(Name, utf8),
                        atom_to_binary(Pattern, utf8))
    end.

match_appender(Name, Appender) ->
    Len = byte_size(Appender),
    case binary:longest_common_prefix([Appender, Name]) of
        0 ->
            false;
        N when N == Len ->
            true;
        Index ->
            Chunks = binary:part(Appender, Index, Len - Index),
            hd(binary:split(Chunks, <<".">>, [global])) == <<"*">>
    end.

server_name(Name) when is_atom(Name) ->
    server_name(atom_to_list(Name));
server_name(Name) when is_list(Name) ->
    list_to_atom(Name).

%% Start Boilerplate

%% TODO: document these functions without repeating the details for each.

-spec(sync_debug/1 :: (string() | #'fastlog.entry'{}) -> 'ok').
sync_debug(E=#'fastlog.entry'{dest=Dest}) ->
    sync_debug(Dest, E);
sync_debug(Format) ->
    sync_debug(fastlog, Format).

-spec(sync_debug/2 :: (atom(), string() | #'fastlog.entry'{}) -> 'ok';
                      (string(), [term()]) -> 'ok').
sync_debug(Name, Entry) when is_atom(Name) ->
    log(Name, {debug, Entry}, sync);
sync_debug(Format, Args) when is_list(Args) ->
    sync_debug(fastlog, Format, Args).

-spec(sync_debug/3 :: (atom(), string(), [term()]) -> 'ok').
sync_debug(Name, Format, Args) when is_list(Args) ->
    log(Name, {debug, Format, Args}, sync).

-spec(sync_info/1 :: (string() | #'fastlog.entry'{}) -> 'ok').
sync_info(E=#'fastlog.entry'{dest=Dest}) ->
    sync_info(Dest, E);
sync_info(Format) ->
    sync_info(fastlog, Format).

-spec(sync_info/2 :: (atom(), string() | #'fastlog.entry'{}) -> 'ok';
                     (string(), [term()]) -> 'ok').
sync_info(Name, Entry) when is_atom(Name) ->
    log(Name, {info, Entry}, sync);
sync_info(Format, Args) when is_list(Args) ->
    sync_info(fastlog, Format, Args).

-spec(sync_info/3 :: (atom(), string(), [term()]) -> 'ok').
sync_info(Name, Format, Args) when is_list(Args) ->
    log(Name, {info, Format, Args}, sync).

-spec(sync_warn/1 :: (string() | #'fastlog.entry'{}) -> 'ok').
sync_warn(E=#'fastlog.entry'{dest=Dest}) ->
    sync_warn(Dest, E);
sync_warn(Format) ->
    sync_warn(fastlog, Format).

-spec(sync_warn/2 :: (atom(), string() | #'fastlog.entry'{}) -> 'ok';
                     (string(), [term()]) -> 'ok').
sync_warn(Name, Entry) when is_atom(Name) ->
    log(Name, {warn, Entry}, sync);
sync_warn(Format, Args) when is_list(Args) ->
    sync_warn(fastlog, Format, Args).

-spec(sync_warn/3 :: (atom(), string(), [term()]) -> 'ok').
sync_warn(Name, Format, Args) when is_list(Args) ->
    sync_warn(Name, {warn, Format, Args}, sync).

-spec(sync_error/1 :: (string() | #'fastlog.entry'{}) -> 'ok').
sync_error(E=#'fastlog.entry'{dest=Dest}) ->
    sync_error(Dest, E);
sync_error(Format) ->
    sync_error(fastlog, Format).

-spec(sync_error/2 :: (atom(), string() | #'fastlog.entry'{}) -> 'ok';
                      (string(), [term()]) -> 'ok').
sync_error(Name, Entry) when is_atom(Name) ->
    log(Name, {error, Entry}, sync);
sync_error(Format, Args) when is_list(Args) ->
    sync_error(fastlog, Format, Args).

-spec(sync_error/3 :: (atom(), string(), [term()]) -> 'ok').
sync_error(Name, Format, Args) when is_list(Args) ->
    log(Name, {error, Format, Args}, sync).

-spec(debug/1 :: (string() | #'fastlog.entry'{}) -> 'ok').
debug(E=#'fastlog.entry'{dest=Dest}) ->
    debug(Dest, E);
debug(Format) ->
    debug(fastlog, Format).

-spec(debug/2 :: (atom(), string() | #'fastlog.entry'{}) -> 'ok';
                 (string(), [term()]) -> 'ok').
debug(Name, Entry) when is_atom(Name) ->
    log(Name, {debug, Entry}, async);
debug(Format, Args) when is_list(Args) ->
    debug(fastlog, Format, Args).

-spec(debug/3 :: (atom(), string(), [term()]) -> 'ok').
debug(Name, Format, Args) when is_list(Args) ->
    log(Name, {debug, Format, Args}, async).

-spec(info/1 :: (string() | #'fastlog.entry'{}) -> 'ok').
info(E=#'fastlog.entry'{dest=Dest}) ->
    info(Dest, E);
info(Format) ->
    info(fastlog, Format).

-spec(info/2 :: (atom(), string() | #'fastlog.entry'{}) -> 'ok';
                (string(), [term()]) -> 'ok').
info(Name, Entry) when is_atom(Name) ->
    log(Name, {info, Entry}, async);
info(Format, Args) when is_list(Args) ->
    info(fastlog, Format, Args).

-spec(info/3 :: (atom(), string(), [term()]) -> 'ok').
info(Name, Format, Args) when is_list(Args) ->
    log(Name, {info, Format, Args}, async).

-spec(warn/1 :: (string() | #'fastlog.entry'{}) -> 'ok').
warn(E=#'fastlog.entry'{dest=Dest}) ->
    warn(Dest, E);
warn(Format) ->
    warn(fastlog, Format).

-spec(warn/2 :: (atom(), string() | #'fastlog.entry'{}) -> 'ok';
                (string(), [term()]) -> 'ok').
warn(Name, Entry) when is_atom(Name) ->
    log(Name, {warn, Entry}, async);
warn(Format, Args) when is_list(Args) ->
    warn(fastlog, Format, Args).

-spec(warn/3 :: (atom(), string(), [term()]) -> 'ok').
warn(Name, Format, Args) when is_list(Args) ->
    log(Name, {warn, Format, Args}, async).

-spec(error/1 :: (string() | #'fastlog.entry'{}) -> 'ok').
error(E=#'fastlog.entry'{dest=Dest}) ->
    error(Dest, E);
error(Format) ->
    fastlog:error(fastlog, Format).

-spec(error/2 :: (atom(), string() | #'fastlog.entry'{}) -> 'ok';
                 (string(), [term()]) -> 'ok').
error(Name, Entry) when is_atom(Name) ->
    log(Name, {error, Entry}, async);
error(Format, Args) when is_list(Args) ->
    fastlog:error(fastlog, Format, Args).

-spec(error/3 :: (atom(), string(), [term()]) -> 'ok').
error(Name, Format, Args) when is_list(Args) ->
    log(Name, {error, Format, Args}, async).
