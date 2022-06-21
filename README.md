# quic_dist

QUIC carrier for Erlang Distribution Protocol.

## Build
    $ rebar3 compile

## Test

Tested on OTP 26 e7e79d8b8b1ec4a97a1940a685b0bb44388c2477

Node names must be in form of 'port@ip'.
EPMD cannot be used as it requires TCP.

```
# shell 1

$ ERL_FLAGS="-proto_dist quic -no_epmd -config sys" rebar3 shell
1> application:ensure_all_started(quic_dist).
{ok,[quicer,quic_dist]}
2> application:set_env([{quic_dist, [{cert, "/path/to/cert.pem"}, {key, "/path/to/key.pem"}]}]).
ok
3> net_kernel:start('5555@127.0.0.1', #{}).
{ok,<0.182.0>}
(5555@127.0.0.1)4> net_adm:ping('4444@127.0.0.1').
pong
(5555@127.0.0.1)5> net_adm:ping('4444@127.0.0.1').
pong
(5555@127.0.0.1)6> net_adm:ping('4444@127.0.0.1').
pong
(5555@127.0.0.1)7> net_adm:ping('4444@127.0.0.1').
pong
(5555@127.0.0.1)8> spawn('4444@127.0.0.1', fun () -> erlang:display(hello) end).
<13800.205.0>
(5555@127.0.0.1)9>
```

```
# shell 2

$ ERL_FLAGS="-proto_dist quic -no_epmd -config sys" rebar3 shell
Eshell V13.0  (abort with ^G)
1> application:ensure_all_started(quic_dist).
{ok,[quicer,quic_dist]}
2> application:set_env([{quic_dist, [{cert, "/path/to/cert.pem"}, {key, "/path/to/key.pem"}]}]).
ok
3> net_kernel:start('4444@127.0.0.1', #{}).
{ok,<0.182.0>}
(4444@127.0.0.1)4> net_adm:ping('5555@127.0.0.1').
pong
(4444@127.0.0.1)5> net_adm:ping('5555@127.0.0.1').
pong
(4444@127.0.0.1)6> net_adm:ping('5555@127.0.0.1').
pong
(4444@127.0.0.1)7> hello

(4444@127.0.0.1)7>
```

## Architecture

![](docs/architecture.drawio.png)

## Supported platforms

Limited to platforms supported by quicer.

## Resources

#### Most important

* https://www.erlang.org/doc/apps/erts/alt_dist.html#distribution-module 
* https://github.com/erlang/otp/blob/OTP-24.2/lib/kernel/src/inet_tcp_dist.erl
* https://github.com/emqx/quic

#### Additional

* https://github.com/hauleth/consulate
* https://www.erlang.org/doc/apps/erts/erl_dist_protocol.html
* https://github.com/potatosalad/otp/pull/1
* https://erlangforums.com/t/rfc-erlang-dist-security-filtering-prototype/1002
