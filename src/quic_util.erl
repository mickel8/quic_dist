-module(quic_util).

-export([call_dist_ctrl/2, hs_data_common/1]).

-include_lib("kernel/include/dist_util.hrl").

call_dist_ctrl(DistCtrl, Msg) ->
    Ref = erlang:monitor(process, DistCtrl),
    DistCtrl ! {Ref, self(), Msg},
    receive
        {Ref, Res} ->
            erlang:demonitor(Ref, [flush]),
            Res;
        {'DOWN', Ref, process, DistCtrl, Reason} ->
            exit({dist_controller_exit, Reason})
    end.

hs_data_common(DistCtrl) ->
    TickHandler = call_dist_ctrl(DistCtrl, tick_handler),
    Socket = call_dist_ctrl(DistCtrl, socket),
    RejectFlags =
        case init:get_argument(quic_dist_reject_flags) of
            {ok, [[Flags]]} ->
                list_to_integer(Flags);
            _ ->
                (#hs_data{})#hs_data.reject_flags
        end,
    #hs_data{f_send = send_fun(),
             f_recv = recv_fun(),
             f_setopts_pre_nodeup = setopts_pre_nodeup_fun(),
             f_setopts_post_nodeup = setopts_post_nodeup_fun(),
             f_getll = getll_fun(),
             f_handshake_complete = handshake_complete_fun(),
             f_address = address_fun(),
             mf_setopts = setopts_fun(DistCtrl, Socket),
             mf_getopts = getopts_fun(DistCtrl, Socket),
             mf_getstat = getstat_fun(DistCtrl, Socket),
             mf_tick = tick_fun(DistCtrl, TickHandler),
             reject_flags = RejectFlags}.

tick_fun(DistCtrl, TickHandler) ->
    fun(Ctrl) when Ctrl == DistCtrl -> TickHandler ! tick end.

getstat_fun(DistCtrl, Socket) ->
    fun(Ctrl) when Ctrl == DistCtrl ->
       case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
           {ok, Stat} ->
               split_stat(Stat, 0, 0, 0);
           Error ->
               Error
       end
    end.

split_stat([{recv_cnt, R} | Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W} | Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P} | Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

setopts_fun(DistCtrl, Socket) ->
    fun(Ctrl, Opts) when Ctrl == DistCtrl -> setopts(Socket, Opts) end.

getopts_fun(DistCtrl, Socket) ->
    fun(Ctrl, Opts) when Ctrl == DistCtrl -> getopts(Socket, Opts) end.

setopts(S, Opts) ->
    case [Opt || {K, _} = Opt <- Opts, K =:= active orelse K =:= deliver orelse K =:= packet]
    of
        [] ->
            inet:setopts(S, Opts);
        Opts1 ->
            {error, {badopts, Opts1}}
    end.

getopts(S, Opts) ->
    inet:getopts(S, Opts).

send_fun() ->
    fun(Ctrlr, Packet) -> call_dist_ctrl(Ctrlr, {send, Packet}) end.

recv_fun() ->
    fun(Ctrlr, Length, Timeout) ->
       case call_dist_ctrl(Ctrlr, {recv, Length, Timeout}) of
           {ok, Bin} when is_binary(Bin) ->
               {ok, binary_to_list(Bin)};
           Other ->
               Other
       end
    end.

getll_fun() ->
    fun(Ctrlr) -> call_dist_ctrl(Ctrlr, getll) end.

address_fun() ->
    fun(Ctrlr, Node) ->
       case call_dist_ctrl(Ctrlr, {address, Node}) of
           {error, no_node} -> %% No '@' or more than one '@' in node name.
               ?shutdown(no_node);
           Res ->
               Res
       end
    end.

setopts_pre_nodeup_fun() ->
    fun(Ctrlr) -> call_dist_ctrl(Ctrlr, pre_nodeup) end.

setopts_post_nodeup_fun() ->
    fun(Ctrlr) -> call_dist_ctrl(Ctrlr, post_nodeup) end.

handshake_complete_fun() ->
    fun(Ctrlr, Node, DHandle) -> call_dist_ctrl(Ctrlr, {handshake_complete, Node, DHandle})
    end.