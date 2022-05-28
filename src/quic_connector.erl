-module(quic_connector).

-export([connector_loop/6]).

-include_lib("kernel/include/dist_util.hrl").
-include_lib("kernel/include/logger.hrl").

%% Set the distribution protocol version statically (the different values
%% are listed in epmd.mk). All nodes are expected to use the same version
%% when using this distribution, to avoid the need for epmd.
-undef(ERL_DIST_VER).
-ifdef(ERL_DIST_VER_6).
%% Set it to 6 when supporting 32-bit big Creation numbers
-define(ERL_DIST_VER, 6).
-else.
%% Set it to 5 when supporting Creation numbers in the 1..3 range
-define(ERL_DIST_VER, 5).
-endif.

-import(error_logger, [error_msg/2]).

connector_loop(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    erlang:display("Starting connector loop"),
    [Name, Address] = quic_util:splitnode(Node, LongOrShortNames),
    erlang:display("After splitnode"),
    case inet:getaddr(Address, inet) of
        {ok, _Ip} ->
            Ip2 = 
                if Node == 'x@michal' ->
                    {127, 0, 0, 1};
                true ->
                    {127, 0, 0, 2}
                end,
            Port = 55555,
            Timer = dist_util:start_timer(SetupTime),
            case true of
                true  ->
                    dist_util:reset_timer(Timer),
                    % erlang:display("Connecting to ~p:~p", [Ip2, Port]),
                    case quicer:connect(Ip2, Port, [{alpn, ["sample"]}], 5000) of
                        {ok, Conn} ->
                            erlang:display("Connected. Creating stream"),
                            {ok, Stream} = quicer:start_stream(Conn, []),
                            {ok, 4} = quicer:send(Stream, <<"ping">>),
                            receive {quic, <<"pong">>, Stream, _, _, _} -> ok end,
                            erlang:display("Received pong"),
                            DistCtrl = quic_dist_cntrlr:spawn_dist_cntrlr(Stream),
                            quicer:controlling_process(Stream, DistCtrl),
                            HSData0 = quic_util:hs_data_common(DistCtrl),
                            erlang:display("After hs data common"),
                            HSData =
                                HSData0#hs_data{kernel_pid = Kernel,
                                                other_node = Node,
                                                this_node = MyNode,
                                                socket = DistCtrl,
                                                timer = Timer,
                                                this_flags = 0,
                                                other_version = ?ERL_DIST_VER,
                                                request_type = Type},
                            erlang:display("Starting handshake"),
                            dist_util:handshake_we_started(HSData);
                        Error ->
                            %% Other Node may have closed since
                            %% port_please !
                            % erlang:display("Couldn't connect to other node (~p). Reason: ~p.~n",
                                %    [Node, Error]),
                            ?shutdown(Node)
                    end;
                Error ->
                    % erlang:display("port_please (~p) failed. Reason (~p) ~n", [Node, Error]),
                    ?shutdown(Node)
            end;
        _Other ->
            % erlang:display("inet_getaddr(~p) "
            %        "failed (~p).~n",
            %        [Node, _Other]),
            ?shutdown(Node)
    end.