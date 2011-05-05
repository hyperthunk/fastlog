# Fast Erlang/OTP Logging Support

This small OTP application aims to bring together several utilities to provide
fast and efficient logging support for Erlang/OTP applications. It is primarily
intended as a wrapper around `error_logger`, so you can continue to take
advantage of enhancements for SASL logging provided by tools like
[riak_err](https://github.com/basho/riak_err). Support for *disk_log* based
logging, and choice between sync and async is in the pipeline.

## Usage

Include it in your deps (or stick on your `ERL_LIBS` path) and use at your leisure.

```erlang
%% rebar.config
{deps, [{fastlog, ".*", {git, "git@github.com:hyperthunk/fastlog.git", "master"}}]}.
```

## API Quickstart

```erlang
demo() ->
    %% start the app
    fastlog:start(),

    %% configure your own application
    fastlog:configure(myapp),

    %% add a new (named) logger
    fastlog:add_logger('net.kit.event.*', [{level, debug}]),

    %% update the level of an existing logger
    fastlog:set_level(my.logger, warn),

    %% log statements....
    fastlog:debug("foobar"),
    fastlog:warn("it is ~p~n", [alive]),

    %% log statements to a specific logger....
    fastlog:debug(my.logger, "foobar"),

    %% log using wildcard - this will hit the 'net.kit.event.*'
    %% logger we defined earlier....
    fastlog:warn(net.kit.event.consumer, "I am the consumer....~n"),
```

## Configuration Options

You can configure fastlog in your application (or release) config and load it
using the `fastlog:configure/1` function. Here's an example:

```erlang
%% in app.config
{webapp, [
    {webconfig, [
        {ip, "127.0.0.1"},
        {port, 8080}
    ]},
    {fastlog, [
        {'web.*', [{level, warn}]},
        {'web.info.event.*', [{level, debug}]}
    ]}
]}
```

## Status and Version Management

This project is in alpha at the moment. We will use semantic versioning.
