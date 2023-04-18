# ekeyx

[![Build Status](https://github.com/sg2342/ekeyx/workflows/Common%20Test/badge.svg)](https://github.com/sg2342/ekeyx/actions?query=branch%3Amain+workflow%3A"Common+Test")

Erlang reimplementation of the Elixir library [KeyX](https://github.com/elcritch/keyx).

byte-compatible to Hashicorp Vault's implementation of Shamir's Secret Sharing (SSS) algorithm.

## Build

    $ rebar3 compile

## Test

    $ ERL_AFLAGS="-enable-feature maybe_expr" rebar3 as test check

## Interop Test

code in [keyx_golang](./keyx_golang) is a minimal command line interface to the golang code in [vault](https://github.com/hashicorp/vault/) 

    $ rebar3 shell
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling ekeyx
    Erlang/OTP 25 [erts-13.0.4] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [jit:ns] [dtrace] [sharing-preserving]
    
    Eshell V13.0.4  (abort with ^G)
    1> [] = os:cmd("cd keyx_golang; go build").
    []
    2> L = os:cmd("./keyx_golang/keyx split 5 2 DEADBEEFDEADBEEF").
    "89a0e17a7f82bf8a01\ne3486a38b0304b63f5\n04dda656bc456d32d3\n3dfd7cfa90ba5209ec\n0018a82dfe27a78519\n"
    3> [S1, S2 | _] = binary:split(list_to_binary(L), <<"\n">>, [global]).
    [<<"89a0e17a7f82bf8a01">>,<<"e3486a38b0304b63f5">>,
     <<"04dda656bc456d32d3">>,<<"3dfd7cfa90ba5209ec">>,
     <<"0018a82dfe27a78519">>,<<>>]
    4> binary:encode_hex(ekeyx:recover_secret([S1, S2], base16)).
    <<"DEADBEEFDEADBEEF">>
