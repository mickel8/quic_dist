-module(quic_connector).

-export([connector_loop/6]).

-include_lib("kernel/include/dist_util.hrl").
-include_lib("include/quic_util.hrl").

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

connector_loop(Kernel, Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    [Name, Host] = quic_util:splitnode(Node),
    case inet:getaddr(Host, inet) of
        {ok, Ip} ->
            {Port, []} = string:to_integer(Name),
            Timer = dist_util:start_timer(SetupTime),
            case true of
                true  ->
                    dist_util:reset_timer(Timer),
                    ?qd_debug("~p", [{connecting, Ip, Port}]),
                    case quicer:connect(Ip, Port, [{alpn, ["sample"]}], 5000) of
                        {ok, Conn} ->
                            ?qd_debug("Connected. Creating stream"),
                            {ok, Stream} = quicer:start_stream(Conn, []),
                            ?qd_debug("Sending random ping just to trigger creating stream"),
                            {ok, 4} = quicer:send(Stream, <<"ping">>),
                            receive {quic, <<"pong">>, Stream, _, _, _} -> ok end,
                            ?qd_debug("Received pong"),
                            DistCtrl = quic_dist_cntrlr:spawn_dist_cntrlr(Conn, Stream),
                            quic_util:call_dist_ctrl(DistCtrl, {supervisor, self()}),
                            quic_util:flush_controller(DistCtrl, {Conn, Stream}),
                            quicer:controlling_process(Stream, DistCtrl),
                            quic_util:flush_controller(DistCtrl, {Conn, Stream}),
                            HSData0 = quic_util:hs_data_common(DistCtrl),
                            HSData =
                                HSData0#hs_data{kernel_pid = Kernel,
                                                other_node = Node,
                                                this_node = MyNode,
                                                socket = DistCtrl,
                                                timer = Timer,
                                                this_flags = 0,
                                                other_version = ?ERL_DIST_VER,
                                                request_type = Type},
                            ?qd_debug("Starting handshake"),
                            dist_util:handshake_we_started(HSData);
                        _Error ->
                            %% Other Node may have closed since
                            ?shutdown(Node)
                    end;
                _Error ->
                    ?shutdown(Node)
            end;
        _Other ->
            ?shutdown(Node)
    end.