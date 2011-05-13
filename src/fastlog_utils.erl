%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
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
-module(fastlog_utils).
-export([compile_pattern/1]).
%% -compile({parse_transform, abstract_code}).

compile_pattern(Pattern) ->
    case match_pattern(Pattern) of
        nomatch ->
            undefined;
        {match, Matches} ->
            {Fmt, Idx} = lists:foldl(fun compile_pattern/2,
                                    {Pattern, []}, Matches),
            compile_formatter(Fmt, lists:reverse(Idx))
    end.

compile_pattern([Elem], {SoFar, Idx}) ->
    {Fmt, Entry} = pattern_entry(Elem),
    NextChunk = re:replace(SoFar, Elem, Fmt, [{return, list}]),
    {NextChunk, [Entry|Idx]}.

pattern_entry([$%,Char|[]]) ->
    pattern_entry(Char);
pattern_entry($L) ->
    {"~p", level};
pattern_entry($N) ->
    {"~p", logger};
pattern_entry($m) ->
    {"~p", module};
pattern_entry($f) ->
    {"~p", function};
pattern_entry($a) ->
    {"~p", arity};
pattern_entry($l) ->
    {"~p", line};
pattern_entry($p) ->
    {"~p", pid};
pattern_entry($n) ->
    {"~p", node};
pattern_entry($s) ->
    {"%s", message}.

match_pattern(P) ->
    re:run(P, "%.{1}", [{capture, all, list}, global]).

compile_formatter(Pattern, Idx) ->
    Forms = {'fun',34,
     {clauses,
      [{clause,34,
        [{var,34,varname_for(level, Idx)},
         {tuple,34,
          [{var,34,'_'},
           {var,34,'Message'},
           {var,34,'Args'},
           {var,34,varname_for(logger, Idx)},
           {tuple,35,
            [{var,35,'_'},
             {var,35,varname_for(node, Idx)},
             {var,35,varname_for(pid, Idx)},
             {var,35,varname_for(module, Idx)},
             {var,36,varname_for(function, Idx)},
             {var,36,varname_for(arity, Idx)},
             {var,36,varname_for(line, Idx)}]}]}],
        [],
        [{match,38,
          {var,38,'Pattern'},
          {string,38, Pattern}},
         {match,39, {var,39,'Format'},
          {call,39, {remote,39,{atom,39,re},{atom,39,replace}},
           [{var,39,'Pattern'}, {string,39,"%s"},{var,39,'Message'},
            {cons,39,{tuple,39,[{atom,39,return},
                                {atom,39,list}]},{nil,39}}]}},
         {match,40, {var,40,'Argv'}, 
            abstract(lists:concat([lists:delete(message, Idx), ['Args']]))},
         {tuple,41,[{var,41,'Format'},{var,41,'Argv'}]}]}]}},
    {_,Fun,_} = erl_eval:expr(Forms, []),
    Fun.

abstract([H|[]]) ->
    abstract(H);
abstract([H|T]) ->
    {cons, 0, abstract(H), abstract(T)};
abstract(Other) ->
    {var, 0, list_to_atom(varname_for(Other))}.

varname_for(Item) when is_atom(Item) ->
    [Char|Rest] = atom_to_list(Item),
    [string:to_upper(Char)|Rest].

varname_for(Item, Idx) ->
    case lists:member(Item, Idx) of
        true ->
            list_to_atom(varname_for(Item));
        false ->
            '_'
    end.
