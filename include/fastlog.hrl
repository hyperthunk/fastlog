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

-type(level() :: debug | info | warn | error | off).
-type(logger_spec() :: {ok, pid() | undefined} |
                       {ok, pid() | undefined, term()} |
                       {error, term()}).

-record('fastlog.callsite', {
    node        :: atom(),
    pid         :: pid(),
    module      :: module(),
    function    :: atom(),
    arity       :: integer(),
    line        :: integer()
}).

-record('fastlog.entry', {
    message = ""         :: [char()],
    args    = []         :: [term()],
    dest    = undefined  :: atom(),
    site    = undefined  :: #'fastlog.callsite'{}
}).

-define(LOG(Dest, Msg, Args), #'fastlog.entry'{
    message=Msg, args=Args, dest=Dest,
    site=#'fastlog.callsite'{
        node=node(),
        pid=self(),
        module=?MODULE
    }
}).
-ifdef(FASTLOG_OFF).
-define(DEBUG(_Format, _Args), ok).
-define(INFO(_Format, _Args), ok).
-define(WARN(_Format, _Args), ok).
-define(ERROR(_Format, _Args), ok).
-else.
-include_lib("fastlog/include/fastlog_internal.hrl").
-endif.
