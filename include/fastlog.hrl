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

-record('fastlog.callsite', {
    module      :: module(),
    function    :: atom(),
    arity       :: integer(),
    line        :: integer()
}).

-record('fastlog.entry', {
    message = ""         :: [char()],
    site    = undefined  :: #'fastlog.callsite'{}
}).

%% NB: these macros do some *bonkers* stuff whenever they're expanded. 
%% I would recommend that you DO NOT use them in production.

%% TODO: provide a parse transform to do this instead.....

-define(DEBUG(Format, Args), 
    ?LOG(debug, Format, Args)).
-define(INFO(Format, Args), 
    ?LOG(info, Format, Args)).
-define(WARN(Format, Args), 
    ?LOG(warn, Format, Args)).
-define(ERROR(Format, Args), 
    ?LOG(error, Format, Args)).
-define(LOG(Level, Format, Args),
    {_, {__Log_M, __Log_F, __Log_A}} = 
        process_info(self(), current_function),
    apply(fastlog, Level, [
        list_to_atom(lists:foldl(
            fun(E, Acc) ->
                case Acc of
                    [] ->
                        E;
                    _ ->
                        string:join([E, Acc], ".")
                end
            end, "", 
            lists:reverse(string:tokens(atom_to_list(?MODULE), "_")))),
        "[~p][~p/~p][line:~p] " ++ Format,
        [?MODULE, __Log_F, __Log_A, ?LINE|Args]])).
