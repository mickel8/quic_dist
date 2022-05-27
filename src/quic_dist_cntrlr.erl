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
    erlang:display("Running DistCntrlr loop"),
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
            % erlang:display("~s ~p", ["[DIST] Sending", Packet]),
            Res = quicer:send(Stream, Packet),
            % erlang:display("~s ~p", ["[DIST] Sent", Res]),
            From ! {Ref, Res},
            dist_cntrlr_loop(Stream, TickHandler);

        {Ref, From, getll} ->
            erlang:display("getll"),
            From ! {Ref, {ok, self()}},
            dist_cntrlr_loop(Stream, TickHandler);

        %% Set the Socket options just before the connection is established
        %% for normal data traffic and before nodeup is delivered. A nodeup
        %% message is delivered when a new node is connected.
        {Ref, From, pre_nodeup} ->
            erlang:display("pre nodeup"),
            %% Switch the distribution protocol to a packet header of
            %% 4 bytes which is used to store the length of each packet
            %% sent over the streamed Unix Domain Sockets.
            % Res = inet:setopts(Socket,
            %                    [{active, false},
            %                     {packet, 4}]),
            From ! {Ref, ok},
            dist_cntrlr_loop(Stream, TickHandler);

        %% Set the Socket options just after the connection is established
        %% for normal data traffic and after nodeup is delivered.
        {Ref, From, post_nodeup} ->
            %% Switch the distribution protocol to a packet header of
            %% 4 bytes, as explained above.
            %% The previous pre_nodeup case should normally be enough.
            % Res = inet:setopts(Socket,
            %                    [{active, false},
            %                     {packet, 4}]),
            erlang:display("post nodeup"),
            From ! {Ref, ok},
            dist_cntrlr_loop(Stream, TickHandler);



        %% Receive a packet of Length bytes, within Timeout milliseconds
        {Ref, From, {recv, Length, Timeout}} ->
            erlang:display(["Receiving", Length, Timeout]),
            % erlang:display("~s ~p ~p", ["[DIST] Receiving", Length, Timeout]),
            % TODO use Timeout
            % erlang:display("~s ~p", ["process", process_info(self(), messages)]),
            receive
                {quic, Msg, _, _, _, _} ->
                    % erlang:display("~s ~p", ["[DIST] Received", Msg]),
                    From ! {Ref, {ok, Msg}}
                % Other ->
                %     erlang:display("~s, ~p", ["OTHER", Other])
            end,
            dist_cntrlr_loop(Stream, TickHandler);
        
        {Ref, From, {handshake_complete, _Node, _DHandle}} ->
            erlang:display("[DIST] Handshake completed!!!"),
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
    % erlang:display("~p~n", [{?MODULE, tick_handler, self()}]),
    receive
        tick ->
            %% May block due to busy port...
            Res = quicer:send(Stream, "");
            % erlang:display("~s, ~p", ["Sent tick", Res]);
        _ ->
            ok
    end,
    tick_handler(Stream).