%%-define(quic_dist_debug, true).

-ifdef(quic_dist_debug).
-define(quic_debug(Term), erlang:display(Term)).
-else.
-define(quic_debug(Term), ok).
-endif.