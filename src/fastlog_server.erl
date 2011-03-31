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
%% TODO: support for disk_log
%%
%% -----------------------------------------------------------------------------

-module(fastlog_server).
-behaviour(gen_server2).

-spec(debug/1 :: (string()) -> 'ok').
-spec(debug/2 :: (string(), [any()]) -> 'ok').
-spec(info/1  :: (string()) -> 'ok').
-spec(info/2  :: (string(), [any()]) -> 'ok').
-spec(warn/1  :: (string()) -> 'ok').
-spec(warn/2  :: (string(), [any()]) -> 'ok').
-spec(error/1 :: (string()) -> 'ok').
-spec(error/2 :: (string(), [any()]) -> 'ok').

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start/0
        ,start/1
        ,start_link/0
        ,start_link/1
        ,debug/1
        ,debug/2
        ,info/1
        ,info/2
        ,warn/1
        ,warn/2
        ,error/1
        ,error/2]).

-type(mode() :: on | off).

-record(state, {
    debug = off :: mode(),
    info  = off :: mode(),
    warn  = off :: mode(),
    error = on  :: mode()
}).

-spec(start/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
%% @doc starts erlxsl_fast_log (standalong) with the default options.
start() ->
    start([]).

-spec(start/1 :: ([{atom(), term()}]) -> {'ok', pid()} | 'ignore' | {'error', any()}).
%% @doc starts erlxsl_fast_log (standalone) with the supplied options.
start(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

-spec(start_link/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
%% @doc starts erlxsl_fast_log with the default options.
start_link() ->
    start_link([]).

-spec(start_link/1 :: ([{atom(), term()}]) -> {'ok', pid()} | 'ignore' | {'error', any()}).
%% @doc starts erlxsl_fast_log with the supplied options, for supervision tree membership.
%% Options = {verbose, on | off} %% turn on verbose logging (i.e., log all messages)
%%                        {debug, on | off} %% turn on debug logging
%%                        {info, on | off} %% turn on infomational logging
%%                        {warn, on | off} %% turn on warning logging
%%                        {error, on | off} %% turn on error logging
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

debug(Format) ->
    gen_server:cast(?MODULE, {debug, Format}).

debug(Format, Args) when is_list(Args) ->
    gen_server:cast(?MODULE, {debug, Format, Args}).

info(Format) ->
    gen_server:cast(?MODULE, {info, Format}).

info(Format, Args) when is_list(Args) ->
    gen_server:cast(?MODULE, {info, Format, Args}).

warn(Format) ->
    gen_server:cast(?MODULE, {warn, Format}).

warn(Format, Args) when is_list(Args) ->
    gen_server:cast(?MODULE, {warn, Format, Args}).

error(Format) ->
    gen_server:cast(?MODULE, {error, Format}).

error(Format, Args) when is_list(Args) ->
    gen_server:cast(?MODULE, {error, Format, Args}).

%%--------------------------------------------------------------------

init(Options) ->
    Verbose = proplists:get_value(verbose, Options, on),
    {ok, #state{
        debug = proplists:get_value(debug, Options, Verbose),
        info    = proplists:get_value(info, Options, Verbose),
        warn    = proplists:get_value(warn, Options, Verbose),
        error = proplists:get_value(error, Options, Verbose)
    }}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({debug, _}, #state{debug=off}=State) ->
    {noreply, State};
handle_cast({debug, Format}, State) ->
    error_logger:info_msg(Format, []),
    {noreply, State};
handle_cast({debug, _, _}, #state{debug=off}=State) ->
    {noreply, State};
handle_cast({debug, Format, Args}, State) ->
    error_logger:info_msg(Format, Args),
    {noreply, State};
handle_cast({info, _}, #state{debug=off}=State) ->
    {noreply, State};
handle_cast({info, Format}, State) ->
    error_logger:info_msg(Format, []),
    {noreply, State};
handle_cast({info, _, _}, #state{info=off}=State) ->
    {noreply, State};
handle_cast({info, Format, Args}, State) ->
    error_logger:info_msg(Format, Args),
    {noreply, State};
handle_cast({warn, _}, #state{warn=off}=State) ->
    {noreply, State};
handle_cast({warn, Format}, State) ->
    error_logger:warning_msg(Format, []),
    {noreply, State};
handle_cast({warn, _, _}, #state{warn=off}=State) ->
    {noreply, State};
handle_cast({warn, Format, Args}, State) ->
    error_logger:warning_msg(Format, Args),
    {noreply, State};
handle_cast({error, _}, #state{error=off}=State) ->
    {noreply, State};
handle_cast({error, Format}, State) ->
    error_logger:error_msg(Format, []),
    {noreply, State};
handle_cast({error, _, _}, #state{error=off}=State) ->
    {noreply, State};
handle_cast({error, Format, Args}, State) ->
    error_logger:error_msg(Format, Args),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({driver_log, {Level, Format}}, State) ->
    handle_cast({Level, Format}, State);
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
