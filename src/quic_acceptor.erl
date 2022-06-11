-module(quic_acceptor).

-export([acceptor_loop/2]).

-include_lib("include/quic_util.hrl").

acceptor_loop(Kernel, Listen) ->
    ?quic_debug("Waiting for new connection"),
    case quicer:accept(Listen, []) of
        {ok, Conn} ->
            ?quic_debug("New connection, accepting"),
            ?quic_debug("Performing QUIC handshake"),
            {ok, Conn} = quicer:handshake(Conn, 5000),
            ?quic_debug("Accepting stream"),
            {ok, Stream} = quicer:accept_stream(Conn, []),
            receive {quic, <<"ping">>, Stream, _, _, _} -> ok end,
            ?quic_debug("Received random ping. Responding with pong"),
            {ok, 4} = quicer:send(Stream, <<"pong">>),
            DistCtrl = quic_dist_cntrlr:spawn_dist_cntrlr(Conn, Stream),
            quicer:controlling_process(Stream, DistCtrl),
            Kernel ! {accept, self(), DistCtrl, inet, udp},
            receive
                {Kernel, controller, SupervisorPid} ->
                    Ref = erlang:monitor(process, DistCtrl),
                    DistCtrl ! {Ref, self(), {supervisor, SupervisorPid}},
                    receive
                        {Ref, Res} ->
                            erlang:demonitor(Ref, [flush]),
                            Res;
                        {'DOWN', Ref, process, DistCtrl, Reason} ->
                            exit({dist_controller_exit, Reason})
                    end,
                    SupervisorPid ! {self(), controller};
                {Kernel, unsupported_protocol} ->
                    exit(unsupported_protocol)
            end,
            acceptor_loop(Kernel, Listen);
        Error ->
            exit(Error)
    end.
