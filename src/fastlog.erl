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
        ,set_level/1
        ,get_level/0
        ,debug/1
        ,debug/2
        ,info/1
        ,info/2
        ,warn/1
        ,warn/2
        ,error/1
        ,error/2]).

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

%% ===================================================================
%% Public API Functions
%% ===================================================================

%% @doc Starts the fastlog application
start() ->
    appstart:start(fastlog).

%% @doc Stops the fastlog application
stop() ->
    application:stop(fastlog).

%% @doc Gets the current log level
get_level() ->
    gen_server:call(fastlog_server, get_level).

%% @doc Sets the current log level
set_level(Lvl) ->
    gen_server:call(fastlog_server, {set_level, Lvl}).

debug(Format) ->
    gen_server:cast(fastlog_server, {debug, Format}).

debug(Format, Args) when is_list(Args) ->
    gen_server:cast(fastlog_server, {debug, Format, Args}).

info(Format) ->
    gen_server:cast(fastlog_server, {info, Format}).

info(Format, Args) when is_list(Args) ->
    gen_server:cast(fastlog_server, {info, Format, Args}).

warn(Format) ->
    gen_server:cast(fastlog_server, {warn, Format}).

warn(Format, Args) when is_list(Args) ->
    gen_server:cast(fastlog_server, {warn, Format, Args}).

error(Format) ->
    gen_server:cast(fastlog_server, {error, Format}).

error(Format, Args) when is_list(Args) ->
    gen_server:cast(fastlog_server, {error, Format, Args}).
