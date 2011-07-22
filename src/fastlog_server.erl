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

-include("fastlog.hrl").

-define(DEFAULT, fastlog).
-define(DEFAULT_PATTERN, "[%n] [%p] [%L] [%m] [%f] [line:%l] %s").

-type(mode() :: on | off).

-record(state, {
    name        :: atom(),
    debug = off :: mode(),
    info  = off :: mode(),
    warn  = off :: mode(),
    error = on  :: mode(),
    handler     :: atom(),
	formatter   :: function()
}).

%%
%% Public API
%%

-spec(start/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
%% @doc starts erlxsl_fast_log (standalong) with the default options.
start() ->
    start([]).

-spec(start/1 :: ([{atom(), term()}]) -> {'ok', pid()} | 'ignore' | {'error', any()}).
%% @doc starts erlxsl_fast_log (standalone) with the supplied options.
start(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Options], []).

-spec(start_link/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
%% @doc starts erlxsl_fast_log with the default options.
start_link() ->
    start_link([{name, ?MODULE}]).

-spec(start_link/1 :: ([{atom(), term()}]) -> {'ok', pid()} | 'ignore' | {'error', any()}).
%% @doc starts erlxsl_fast_log with the supplied options, for supervision tree membership.
%% Options = {level, debug | info | warn | error}
start_link(Options) ->
    DefaultName = ?DEFAULT,
    {Name, StartArgs} = case kvc:value(name, Options, DefaultName) of
        [] -> {DefaultName, [{name, DefaultName}|Options]};
        Value -> {Value, Options}
    end,
    gen_server:start_link({local, Name},
                            ?MODULE, [StartArgs], []).

%%
%% gen_server API
%%

init([Options]) ->
    Lvl = proplists:get_value(level, Options, error),
    Name = proplists:get_value(name, Options, ?DEFAULT),
    Handler = proplists:get_value(handler, Options, fastlog_logger),
    Pattern = proplists:get_value(pattern, Options, ?DEFAULT_PATTERN),
    Formatter = fastlog_utils:compile_pattern(Pattern),
    State = set_level(Lvl, #state{name=Name, handler=Handler,
                                  formatter=Formatter}),
    {ok, State}.

handle_call(get_level, _From, State) ->
    {reply, get_level(State), State};
handle_call({set_level, Lvl}, _From, State) ->
    {reply, {ok, Lvl}, set_level(Lvl, State)};
handle_call({_Lvl, _Entry}=Msg, _From, State) ->
    handle_cast(Msg, State),
    {reply, ok, State};
handle_call({_Lvl, _Fmt, _Args}=Msg, _From, State) ->
    handle_cast(Msg, State),
    {reply, ok, State}.

handle_cast({debug, _}, #state{debug=off}=State) ->
    {noreply, State};
handle_cast({debug, Entry}, State) ->
    log(warn, Entry, State),
    {noreply, State};
handle_cast({debug, _, _}, #state{debug=off}=State) ->
    {noreply, State};
handle_cast({debug, Format, Args}, State) ->
    log(debug, Format, Args, State),
    {noreply, State};
handle_cast({info, _}, #state{info=off}=State) ->
    {noreply, State};
handle_cast({info, Entry}, State) ->
    log(info, Entry, State),
    {noreply, State};
handle_cast({info, _, _}, #state{info=off}=State) ->
    {noreply, State};
handle_cast({info, Format, Args}, State) ->
    log(info, Format, Args, State),
    {noreply, State};
handle_cast({warn, _}, #state{warn=off}=State) ->
    {noreply, State};
handle_cast({warn, Entry}, State) ->
    log(warn, Entry, State),
    {noreply, State};
handle_cast({warn, _, _}, #state{warn=off}=State) ->
    {noreply, State};
handle_cast({warn, Format, Args}, State) ->
    log(warn, Format, Args, State),
    {noreply, State};
handle_cast({error, _}, #state{error=off}=State) ->
    {noreply, State};
handle_cast({error, Entry}, State) ->
    log(error, Entry, State),
    {noreply, State};
handle_cast({error, _, _}, #state{error=off}=State) ->
    {noreply, State};
handle_cast({error, Format, Args}, State) ->
    log(error, Format, Args, State),
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

%%
%% Internal API
%%

log(Lvl, #'fastlog.entry'{}=Entry,
         #state{handler=Handler, formatter=Formatter})
                                        when is_function(Formatter, 2) ->
    {Format, Args} = Formatter(Lvl, Entry),
    Handler:log(Lvl, Format, Args);
log(Lvl, #'fastlog.entry'{message=Msg, args=Args},
         #state{handler=Handler, formatter=undefined}) ->
    Handler:log(Lvl, Msg, Args);
log(Lvl, Entry, #state{handler=Handler}) when is_list(Entry) ->
    Handler:log(Lvl, Entry, []).

log(Lvl, Entry, Args, #state{handler=Handler}) when is_list(Entry) ->
    Handler:log(Lvl, Entry, Args).

set_level(Lvl, State) ->
    {D2, I2, W2, E2} = set_level(Lvl),
    State#state{debug=D2, info=I2, warn=W2, error=E2}.

set_level(Lvl) ->
    case Lvl of
        debug -> {on,  on, 	on,  on};
        info ->  {off, on, 	on,  on};
        warn ->  {off, off, on,  on};
        error -> {off, off, off, on};
        none ->  {off, off, off, off}
    end.

get_level(#state{ debug=on }) ->            debug;
get_level(#state{ debug=off, info=on }) ->  info;
get_level(#state{ info=off, warn=on }) ->   warn;
get_level(#state{ warn=off, error=on }) ->  error;
get_level(#state{ error=off }) ->           off.
