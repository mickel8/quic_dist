-module(quic_dist_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        tc_connect    
    ].

tc_connect(_Config) ->
    {ok, Pid} = net_kernel:start([x, shortnames]),
    {ok, _Node} = peer:start(#{name => y, longnames => false, args => ["-proto_dist", "quic", "-no_epmd"]}),
    erlang:displa("ping"),
    ?assertEqual(ok, ok),
    ok.