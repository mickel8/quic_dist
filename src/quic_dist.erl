-module(quic_dist).

-export([listen/1, listen/2, address/0, accept/1, accept_connection/5, setup/5, close/1,
         select/1, setopts/2, getopts/2]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/logger.hrl").

listen(Name) ->
    {ok, Host} = inet:gethostname(),
    listen(Name, Host).

listen(Name, Host) ->
    ?LOG_INFO("Ping"),
    erlang:display("Ping"),
    application:ensure_all_started(quicer),
    {MinPort, _MaxPort} = get_port_range(),
    LOptions = [{cert, "cert.pem"}, {key, "key.pem"}, {alpn, ["sample"]}],
    {ok, L} = quicer:listen(MinPort, LOptions),
    {ok, {Address, _Port}} = quicer:sockname(L),
    NetAddress =
        #net_address{address = Address,
                     host = Host,
                     protocol = udp,
                     family = inet},
    Creation = 1,
    {ok, {L, NetAddress, Creation}}.

get_port_range() ->
    {50000, 60000}.

address() ->
    ok.

accept(_Listen) ->
    ok.

accept_connection(_AcceptorPid, _DistCtrl, _MyNode, _Allowed, _SetupTime) ->
    ok.

setup(_Node, _Type, _MyNode, _LongOrShortNames, _SetupTime) ->
    ok.

close(_Listen) ->
    ok.

select(_NodeName) ->
    % TODO
    true.

setopts(_Listen, _Opts) ->
    ok.

getopts(_Listen, _Opts) ->
    ok.
