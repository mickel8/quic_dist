-module(quic_dist).

-export([listen/1, listen/2, address/0, accept/1, accept_connection/5, setup/5, close/1,
         select/1, setopts/2, getopts/2]).

listen(_Name) ->
    ok.

listen(_Name, _Host) ->
    ok.

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
    ok.

setopts(_Listen, _Opts) ->
    ok.

getopts(_Listen, _Opts) ->
    ok.
