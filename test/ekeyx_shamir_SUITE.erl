-module(ekeyx_shamir_SUITE).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    basic_split/1,
    basic_recover_static_short/1,
    basic_recover_static_long/1,
    basic_split_and_recover_short/1,
    basic_split_and_recover_long/1,
    cover/1
]).

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(crypto),
    [{started_applications, Started} | Config].

end_per_suite(Config) ->
    Started = proplists:get_value(started_applications, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)).

all() ->
    [
        basic_split,
        basic_recover_static_short,
        basic_recover_static_long,
        basic_split_and_recover_short,
        basic_split_and_recover_long,
        cover
    ].

basic_split(_Config) ->
    Secret = <<"Test">>,
    Shares = ekeyx_shamir:split_secret(3, 5, Secret),
    5 = length(Shares),
    lists:foreach(
        fun(Share) ->
            true = (size(Share) == (size(Secret) + 1))
        end,
        Shares
    ).

basic_recover_static_short(_Config) ->
    Shares = lists:map(fun binary:decode_hex/1, [<<"0E4A">>, <<"9954">>]),
    <<"t">> = ekeyx_shamir:recover_secret(Shares).

basic_recover_static_long(_Config) ->
    Shares = lists:map(
        fun binary:decode_hex/1,
        [
            <<"C8EF4C4201">>,
            <<"9673E6A402">>,
            <<"2AF9D99203">>,
            <<"992DC30904">>,
            <<"25A7FC3F05">>
        ]
    ),
    <<"test">> = ekeyx_shamir:recover_secret(Shares).

basic_split_and_recover_short(_Config) ->
    Secret = <<"t">>,
    Shares = ekeyx_shamir:split_secret(2, 2, Secret),
    Secret = ekeyx_shamir:recover_secret(Shares).

basic_split_and_recover_long(_Config) ->
    Secret = <<"super secret">>,
    [S1, S2, S3, S4] = ekeyx_shamir:split_secret(2, 4, Secret),
    Secret = ekeyx_shamir:recover_secret([S1, S2]),
    Secret = ekeyx_shamir:recover_secret([S1, S3]),
    Secret = ekeyx_shamir:recover_secret([S1, S4]),
    Secret = ekeyx_shamir:recover_secret([S2, S3]),
    Secret = ekeyx_shamir:recover_secret([S2, S4]),
    Secret = ekeyx_shamir:recover_secret([S3, S4]).

cover(_Config) ->
    {throw, "duplicate shares"} =
        try
            ekeyx_shamir:recover_secret([<<"0E4A">>, <<"9954">>, <<"9954">>])
        catch
            C1:E1 -> {C1, E1}
        end,
    {throw, "shares must match in size"} =
        try
            ekeyx_shamir:recover_secret([<<"0E">>, <<"9954">>])
        catch
            C2:E2 -> {C2, E2}
        end.
