{application,
 fastlog,
 [{description,[]},
  {vsn,"0.0.2"},
  {modules,[fastlog,fastlog_server,fastlog_sup]},
  {registered,[fastlog_server]},
  {applications,[kernel,stdlib,sasl,riak_err]},
  {mod,{appstart_loader,[]}},
  {env,[
    {appstart,[
        {startup, [fastlog_sup, start_link]}
    ]}
  ]}]}.