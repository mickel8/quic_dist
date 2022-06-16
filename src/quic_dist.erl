-module(quic_dist).

-export([listen/1, listen/2, address/0, accept/1, accept_connection/5, setup/5, close/1,
         select/1, setopts/2, getopts/2]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("include/quic_util.hrl").

listen(Name) ->
    {ok, Host} = inet:gethostname(),
    listen(Name, Host).

listen(Name, Host) ->
    ?qd_debug("Starting listening"),
    application:ensure_all_started(quicer),
    {_MinPort, _MaxPort} = get_port_range(),
    LOptions = [{cert, "/home/michal/Repos/qerldist/cert.pem"}, {key, "/home/michal/Repos/qerldist/key.pem"}, {alpn, ["sample"]}, {peer_bidi_stream_count, 20}],
    
    {ok, LHandle} =
    if Name == 'x' ->
        quicer:listen("127.0.0.1:55555", LOptions);
    true ->
        quicer:listen("127.0.0.2:55555", LOptions)
    end,

    {ok, Address} = quicer:sockname(LHandle),
    NetAddress =
        #net_address{address = Address,
                     host = Host,
                     protocol = udp,
                     family = inet},
    Creation = 1,
    {ok, {LHandle, NetAddress, Creation}}.

get_port_range() ->
    {50000, 60000}.

address() ->
    {ok, Host} = inet:gethostname(),
    % address field has to remain unset as
    % we are not creating socket here
    #net_address{host = Host,
                 protocol = udp,
                 family = inet}.

accept(Listen) ->
    % priority max is enforced by Erlang documentation
    ?qd_debug("Spawning Acceptor"),
    spawn_opt(quic_acceptor, acceptor_loop, [self(), Listen], [link, {priority, max}]).

accept_connection(AcceptorPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    spawn_opt(quic_supervisor,
              supervisor_loop,
              [self(), AcceptorPid, DistCtrl, MyNode, Allowed, SetupTime],
              [link, {priority, max}]).

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?qd_debug("Spawning connector"),
    spawn_opt(quic_connector,
              connector_loop,
              [self(), Node, Type, MyNode, LongOrShortNames, SetupTime],
              [link, {priority, max}]).

close(_Listen) ->
    ok.

select(_NodeName) ->
    % TODO
    true.

setopts(_Listen, _Opts) ->
    ok.

getopts(_Listen, _Opts) ->
    ok.
