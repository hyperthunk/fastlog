%% common_test suite for fastlog_logger

-module(fastlog_logger_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("../include/fastlog.hrl").

-compile(export_all).

debug_logging_delegation() ->
    [{userdata,[{doc, "fastlog_logger delegates debug to error_logger."}]}].

debug_logging_delegation(_Config) ->
    Format = "This is my message: ~s~n",
    Args = ["Hello World!"],
    meck:expect(error_logger, info_msg, expect(Format, Args)),
    fastlog_logger:log(debug, Format, Args),
    ?assertThat(meck:validate(error_logger), is(true)).

info_logging_delegation() ->
    [{userdata,[{doc, "fastlog_logger delegates info to error_logger."}]}].

info_logging_delegation(_Config) ->
    Format = "This is my message: ~s~n",
    Args = ["Hello World!"],
    meck:expect(error_logger, info_msg, expect(Format, Args)),
    fastlog_logger:log(info, Format, Args),
    ?assertThat(meck:validate(error_logger), is(true)).

warn_logging_delegation() ->
    [{userdata,[{doc, "fastlog_logger delegates warn to error_logger."}]}].

warn_logging_delegation(_Config) ->
    Format = "This is my message: ~s~n",
    Args = ["Hello World!"],
    meck:expect(error_logger, warning_msg, expect(Format, Args)),
    fastlog_logger:log(warn, Format, Args),
    ?assertThat(meck:validate(error_logger), is(true)).

error_logging_delegation() ->
    [{userdata,[{doc, "fastlog_logger delegates error to error_logger."}]}].

error_logging_delegation(_Config) ->
    Format = "This is my message: ~s~n",
    Args = ["Hello World!"],
    meck:expect(error_logger, error_msg, expect(Format, Args)),
    fastlog_logger:log(error, Format, Args),
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
