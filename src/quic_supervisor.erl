-module(quic_supervisor).

-export([supervisor_loop/6]).

-import(quic_util, [call_dist_ctrl/2]).

-include_lib("kernel/include/dist_util.hrl").
-include_lib("include/quic_util.hrl").

supervisor_loop(Kernel, Acceptor, DistCtrl, MyNode, Allowed, SetupTime) ->
    receive
        {Acceptor, controller} ->
            Timer = dist_util:start_timer(SetupTime),
            case check_ip(DistCtrl) of
                true ->
                    HSData0 = quic_util:hs_data_common(DistCtrl),
                    HSData =
                        HSData0#hs_data{kernel_pid = Kernel,
                                        this_node = MyNode,
                                        socket = DistCtrl,
                                        timer = Timer,
                                        this_flags = 0,
                                        allowed = Allowed},
                    ?quic_debug("Starting handshake"),
                    dist_util:handshake_other_started(HSData);
                {false, _IP} ->
                    ?shutdown(no_node)
            end
    end.

check_ip(_DistCtrl) ->
    true.
