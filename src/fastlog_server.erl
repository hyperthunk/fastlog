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

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start/0
        ,start/1
        ,start_link/0
        ,start_link/1]).

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
%% Options = {level, debug | info | warn | error}
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%--------------------------------------------------------------------

init(Options) ->
    Lvl = proplists:get_value(level, Options, error),
    {ok, set_level(Lvl, #state{})}.

handle_call({set_level, Lvl}, _From, State) ->
    {reply, {ok, Lvl}, set_level(Lvl, State)};
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

set_level(Lvl, State) ->
    {D2, I2, W2, E2} = set_level(Lvl),
    State#state{debug=D2, info=I2, warn=W2, error=E2}.

set_level(Lvl) ->
    case Lvl of
        debug ->
            {on, on, on, on};
        info ->
            {off, on, on, on};
        warn ->
            {off, off, on, on};
        error ->
            {off, off, off, on};
        none ->
            {off, off, off, off}
    end.

