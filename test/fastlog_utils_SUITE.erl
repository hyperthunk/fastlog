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
%% common_test suite for fastlog_utils
%% -----------------------------------------------------------------------------

-module(fastlog_utils_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("../include/fastlog.hrl").

%% custom matchers
is_function(Arity) ->
    match_mfa(erlang, is_function, [Arity]).

test_multiple_indexes_in_pattern() ->
    [{userdata,[{doc,"Parse a format/pattern with multiple entries."}]}].

test_multiple_indexes_in_pattern(_Config) ->
    Fun = fastlog_utils:compile_pattern("[%p on %n][%L] [%m] [%f/%a - line:%l] %s"),
    {F,I} = Fun(debug, #'fastlog.entry'{
        message="Hello ~s World\n",
        args=["cruel"],
        dest=?MODULE,
        site=#'fastlog.callsite'{
            node=node(),
            pid=self(),
            module=?MODULE,
            function=test_multiple_indexes_in_pattern,
            arity=1,
            line=53
        }
    }),
    ?assertThat(F, equal_to("[~p on ~p][~p] [~p] [~p/~p - line:~p] Hello ~s World\n")),
    ?assertThat(I, equal_to([self(), 
                             node(), 
                             debug, 
                             ?MODULE, 
                             test_multiple_indexes_in_pattern, 
                             1, 53, "cruel"])).

all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].
