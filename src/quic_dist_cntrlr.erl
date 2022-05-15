-module(quic_dist_cntrlr).

-export([dist_cntrlr_loop/1, spawn_dist_cntrlr/1]).
-include_lib("kernel/include/logger.hrl").

%% COPIED FROM gen_tcp_dist.erl example
%%
%% In order to avoid issues with lingering signal binaries
%% we enable off-heap message queue data as well as fullsweep
%% after 0. The fullsweeps will be cheap since we have more
%% or less no live data.
-define(DIST_CNTRL_COMMON_SPAWN_OPTS,
        [{message_queue_data, off_heap}, {fullsweep_after, 0}]).

spawn_dist_cntrlr(Stream) ->
    ?LOG_DEBUG("Running DistCntrlr loop"),
    spawn_opt(quic_dist_cntrlr,
              dist_cntrlr_loop,
              [Stream],
              [{priority, max}] ++ ?DIST_CNTRL_COMMON_SPAWN_OPTS).

dist_cntrlr_loop(Stream) ->
    receive
        {Ref, From, {supervisor, SupervisorPid}} ->
            Res = link(SupervisorPid),
            From ! {Ref, Res},
            dist_cntrlr_loop(Stream);
        {Ref, From, {handshake_complete, _Node, _DHandle}} ->
            From ! {Ref, ok}
    end.
