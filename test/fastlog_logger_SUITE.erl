%% common_test suite for fastlog_logger

-module(fastlog_logger_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("../include/fastlog.hrl").

-compile(export_all).

test_fastlog_logger() ->
    [{userdata,[{doc, "Testing fastlog_logger delegation to error_logger."}]}].

test_fastlog_logger(_Config) ->
    Format = "This is my message: ~s~n",
    Args = ["Hello World!"],
    meck:expect(error_logger, info_msg, expect(Format, Args)),
    fastlog_logger:log(debug, Format, Args),
    ?assertThat(meck:validate(error_logger), is(true)).

expect(Format, Args) ->
    fun(F, A) ->
        ?assertThat(F, is(equal_to(Format))),
        ?assertThat(A, is(equal_to(Args)))
    end.

init_per_suite(Config) ->
    code:unstick_mod(error_logger),
    application:start(fastlog),
    Config.

end_per_suite(_Config) ->
    application:stop(fastlog),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = meck:new(error_logger),
    Config.

end_per_testcase(_TestCase, Config) ->
    meck:unload(error_logger),
    Config.

all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({expect,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].
