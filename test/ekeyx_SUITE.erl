-module(ekeyx_SUITE).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([generate_and_recover/1, generate_and_recover_base/1]).

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(crypto),
    [{started_applications, Started} | Config].

end_per_suite(Config) ->
    Started = proplists:get_value(started_applications, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)).

all() -> [generate_and_recover, generate_and_recover_base].

generate_and_recover(_Config) ->
    Secret = <<"Test">>,
    [_S1, S2, _S3, _S4, _S5, _S6, S7] =
        ekeyx:generate_shares(2, 7, Secret),
    Secret = ekeyx:recover_secret([S2, S7]).

generate_and_recover_base(_Config) ->
    lists:foreach(
        fun generate_and_recover_base1/1,
        [binary, base16, base64]
    ).

generate_and_recover_base1(Base) ->
    Secret = <<"Test">>,
    L = ekeyx:generate_shares(2, 7, Secret, Base),
    Secret = ekeyx:recover_secret(lists:sublist(L, 2), Base).
