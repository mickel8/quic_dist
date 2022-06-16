%% Set to true to enable QUIC DIST debug logs 
-define(qd_debug, true).

-ifdef(qd_debug).
-define(qd_debug(Msg), io:format("[QUIC_DIST] ~s\n", [Msg])).
-define(qd_debug(Fmt, Args), io:format("[QUIC_DIST] ~s\n", [io_lib:format(Fmt, Args)])).
-else.
-define(qd_debug(Fmt, Args), ok).
-endif.