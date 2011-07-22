-ifdef(FASTLOG_SYNC).
-define(DEBUG(Format, Args), 
    fastlog:sync_debug(Format, Args)).
-define(INFO(Format, Args), 
    fastlog:sync_info(Format, Args)).
-define(WARN(Format, Args), 
    fastlog:sync_warn(Format, Args)).
-define(ERROR(Format, Args), 
    fastlog:sync_error(Format, Args)).
-else.
-define(DEBUG(Format, Args), 
    fastlog:debug(Format, Args)).
-define(INFO(Format, Args), 
    fastlog:info(Format, Args)).
-define(WARN(Format, Args), 
    fastlog:warn(Format, Args)).
-define(ERROR(Format, Args), 
    fastlog:error(Format, Args)).
-endif.
