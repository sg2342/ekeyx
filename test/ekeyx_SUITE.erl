-module(ekeyx_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([generate_and_recover/1, generate_and_recover_base/1]).

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(crypto),
    [{started_applications, Started} | Config].

end_per_suite(Config0) ->
    {value, {_, Started}, Config} =
        lists:keytake(started_applications, 1, Config0),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    Config.

init_per_testcase(_TC, Config) -> Config.

end_per_testcase(_TC, Config) -> Config.

all() -> [generate_and_recover, generate_and_recover_base].

generate_and_recover(_Config) ->
    Secret = <<"Test">>,
    [_S1, S2, _S3, _S4, _S5, _S6, S7] =
        ekeyx:generate_shares(2, 7, Secret),
    Secret = ekeyx:recover_secret([S2, S7]).

generate_and_recover_base(_Config) ->
    Secret = <<"Test">>,
    [_, S2, _, _, _, _, S7] =
        ekeyx:generate_shares(2, 7, Secret, binary),
    Secret = ekeyx:recover_secret([S2, S7], binary),
    [S1, _, _, _, S5, _, _] =
        ekeyx:generate_shares(2, 7, Secret, base16),
    Secret = ekeyx:recover_secret([S1, S5], base16),
    [_, _, S3, S4, _, _, _] =
        ekeyx:generate_shares(2, 7, Secret, base64),
    Secret = ekeyx:recover_secret([S3, S4], base64).
