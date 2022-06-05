-module(quic_dist_cntrlr).

-export([dist_cntrlr_loop/2, spawn_dist_cntrlr/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/net_address.hrl").

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

        {Ref, From, {address, Node}} ->
            Res = case quicer:peername(Stream) of
                      {ok, {_PeerIp, _PeerPort}=Address} ->
                          case quic_util:split_node(atom_to_list(Node), $@, []) of
                              [_,Host] ->
                                  #net_address{address=Address,host=Host,
                                               protocol=udp, family=inet};
                              _ ->
                                  {error, no_node}
                          end
                  end,
            From ! {Ref, Res},
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
        
        {Ref, From, {handshake_complete, _Node, DHandle}} ->
            erlang:display("[DIST] Handshake completed!!!"),
            From ! {Ref, ok},
            %% Handshake complete! Begin dispatching traffic

            %% Use a separate process for dispatching input. This
            %% is not necessary, but it enables parallel execution
            %% of independent work loads at the same time as it
            %% simplifies the implementation.
            InputHandler = spawn_opt(
                             fun() -> dist_controller_input_handler(DHandle,
                                                                    Stream,
                                                                    nil)
                             end,
                             [link] ++ ?DIST_CNTRL_COMMON_SPAWN_OPTS),
            quicer:controlling_process(Stream, InputHandler),
            %% Register the input handler process
            erlang:dist_ctrl_input_handler(DHandle, InputHandler),
            InputHandler ! DHandle,
            process_flag(priority, normal),
            erlang:dist_ctrl_get_data_notification(DHandle),
            dist_controller_output_handler(DHandle, Stream)

    end.

%% ---------------------------------------------------------------------
%% Input handler
%%
%% Dispatch all traffic from the remote node coming to this node through
%% the socket.
%% ---------------------------------------------------------------------
dist_controller_input_handler(DHandle, Stream, Sup) ->
    % link(Sup),
    erlang:display(input_handler),
    receive
        %% Wait for the input handler to be registered before starting
        %% to deliver incoming data.
        DHandle ->
            dist_controller_input_loop(DHandle, Stream, <<>>)
    end.


% dist_controller_input_loop(DHandle, Socket, N) when N =< ?ACTIVE_INPUT/2 ->
%     %% Set the socket in active mode and define the number of received data
%     %% packets that will be delivered as {tcp, Socket, Data} messages.
%     inet:setopts(Socket, [{active, ?ACTIVE_INPUT - N}]),
%     dist_controller_input_loop(DHandle, Socket, ?ACTIVE_INPUT);

dist_controller_input_loop(DHandle, Stream, Acc) ->
    erlang:display(input_loop),
    receive
        %% In active mode, data packets are delivered as messages
        {quic, Data, _, _, _, _} ->
            %% When data is received from the remote node, deliver it
            %% to the local node.
            erlang:display({input_handler, Data}),
            Acc2 = <<Acc/binary, Data/binary>>,
            <<Len:32, Rest/binary>> = Acc2,
            if byte_size(Rest) >= Len ->
                erlang:display({input_handler, len, Len}),
                <<Data2:Len/binary, Rest2/binary>> = Rest,
                erlang:display({enough_data, Data2}),
                erlang:dist_ctrl_put_data(DHandle, Data2),
                dist_controller_input_loop(DHandle, Stream, Rest2);
            true ->
                erlang:display(not_enough_data),
                dist_controller_input_loop(DHandle, Stream, Acc2)
            end;
            % try erlang:dist_ctrl_put_data(DHandle, Data)
            % catch _ : _ -> death_row()
            % end,
            %% Decrease the counter when looping so that the socket is
            %% set with {active, Count} again to receive more data.
            % dist_controller_input_loop(DHandle, Stream);

        %% Connection to remote node terminated
        % {tcp_closed, Socket} ->
        %     exit(connection_closed);

        %% Ignore all other messages
        _ ->
            dist_controller_input_loop(DHandle, Stream, Acc)
    end.

%% ---------------------------------------------------------------------
%% Output handler
%%
%% Dispatch all outgoing traffic from this node to the remote node through
%% the socket.
%% ---------------------------------------------------------------------
dist_controller_output_handler(DHandle, Stream) ->
    erlang:display(output_handler),
    receive
        dist_data ->
            %% Available outgoing data to send from this node
            try dist_controller_send_data(DHandle, Stream)
            catch _ : _ -> death_row()
            end,
            dist_controller_output_handler(DHandle, Stream);

        _ ->
            %% Ignore all other messages
            dist_controller_output_handler(DHandle, Stream)
    end.

dist_controller_send_data(DHandle, Stream) ->
    %% Fetch data from the local node to be sent to the remote node
    erlang:display(send_data),
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            %% Request notification when more outgoing data is available.
            %% A dist_data message will be sent.
            erlang:dist_ctrl_get_data_notification(DHandle);
        Data ->
            % stream_send(Stream, Data),
            RealData = if is_list(Data) ->
                binary:list_to_bin(Data);
            true ->
                Data
            end,
            DataLen = byte_size(RealData),
            erlang:display({data_len, DataLen}),
            LenPlusData = <<DataLen:32, RealData/binary>>,
            erlang:display({output_handler, len_plus_data, LenPlusData}),
            quicer:send(Stream, LenPlusData),
            %% Loop as long as there is more data available to fetch
            dist_controller_send_data(DHandle, Stream)
    end.


%% ---------------------------------------------------------------------
%% death_row
%%
%% When the connection is on its way down, operations begin to fail. We
%% catch the failures and call this function waiting for termination. We
%% should be terminated by one of our links to the other involved parties
%% that began bringing the connection down. By waiting for termination we
%% avoid altering the exit reason for the connection teardown. We however
%% limit the wait to 5 seconds and bring down the connection ourselves if
%% not terminated...
%% ---------------------------------------------------------------------
death_row() ->
    death_row(connection_closed).

death_row(normal) ->
    %% We do not want to exit with normal exit reason since it won't
    %% bring down linked processes...
    death_row();

death_row(Reason) ->
    receive after 5000 -> exit(Reason) end.

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