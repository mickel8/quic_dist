-module(quic_acceptor).

-export([acceptor_loop/2]).

-include_lib("kernel/include/logger.hrl").

acceptor_loop(Kernel, Listen) ->
    ?LOG_DEBUG("Running Acceptor loop"),
    case quicer:accept(Listen, []) of
        {ok, Conn} ->
            ?LOG_DEBUG("New connection, accepting"),
            {ok, Conn} = quicer:handshake(Conn, 5000),
            ?LOG_DEBUG("Performing QUIC handshake"),
            {ok, Stream} = quicer:accept_stream(Conn, []),
            ?LOG_DEBUG("Accepting stream"),
            receive {quic, <<"ping">>, Stream, _, _, _} -> ok end,
            {ok, 4} = quicer:send(Stream, <<"pong">>),
            DistCtrl = quic_dist_cntrlr:spawn_dist_cntrlr(Stream),
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
            erlang:display(Error),
            exit(Error)
    end.
