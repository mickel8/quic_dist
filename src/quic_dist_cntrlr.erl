-module(quic_dist_cntrlr).

-export([dist_cntrlr_loop/2, spawn_dist_cntrlr/1]).
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
    TickHandler = spawn_opt(fun() -> tick_handler(Stream) end,
            %% Spawn on max priority
            [link, {priority, max}] ++ ?DIST_CNTRL_COMMON_SPAWN_OPTS),
    spawn_opt(quic_dist_cntrlr,
              dist_cntrlr_loop,
              [Stream, TickHandler],
              [{priority, max}] ++ ?DIST_CNTRL_COMMON_SPAWN_OPTS).

dist_cntrlr_loop(Stream, TickHandler) ->
    receive
        %% Set Pid as the connection supervisor, link with it and
        %% send the linking result back.
        {Ref, From, {supervisor, SupervisorPid}} ->
            Res = link(SupervisorPid),
            From ! {Ref, Res},
            dist_cntrlr_loop(Stream, TickHandler);

        %% Send the tick handler to the From process
        {Ref, From, tick_handler} ->
            From ! {Ref, TickHandler},
            dist_cntrlr_loop(Stream, TickHandler);

        %% Send the stream to the From process
        {Ref, From, stream} ->
            From ! {Ref, Stream},
            dist_cntrlr_loop(Stream, TickHandler);
        
        %% Send Packet onto the stream and send the result back
        {Ref, From, {send, Packet}} ->
            ?LOG_DEBUG("[DIST] Sending"),
            Res = quicer:send(Stream, Packet),
            From ! {Ref, Res},
            dist_cntrlr_loop(Stream, TickHandler);

        %% Receive a packet of Length bytes, within Timeout milliseconds
        {Ref, From, {recv, Length, Timeout}} ->
            ?LOG_DEBUG("~s ~p", ["[DIST] Receiving", Length]),
            %% TODO use Timeout
            {ok, Res} = quicer:recv(Stream, Length),
            From ! {Ref, Res},
            dist_cntrlr_loop(Stream, TickHandler);
        
        {Ref, From, {handshake_complete, _Node, _DHandle}} ->
            ?LOG_DEBUG("[DIST] Handshake completed!!!"),
            From ! {Ref, ok}
    end.


%% ---------------------------------------------------------------------
%% Tick handler
%%
%%
%% The tick handler process writes a tick message to the socket when it
%% receives a 'tick' request from the connection supervisor.
%% ---------------------------------------------------------------------
tick_handler(Stream) ->
    ?LOG_DEBUG("~p~n", [{?MODULE, tick_handler, self()}]),
    receive
        tick ->
            %% May block due to busy port...
            quicer:send(Stream, "");
        _ ->
            ok
    end,
    tick_handler(Stream).