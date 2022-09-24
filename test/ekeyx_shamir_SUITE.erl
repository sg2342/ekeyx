-module(ekeyx_shamir_SUITE).

-include_lib("common_test/include/ct.hrl").


-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).


-export([basic_split/1, basic_recover_static_short/1,
	 basic_recover_static_long/1, basic_split_and_recover_short/1,
	 basic_split_and_recover_long/1, cover/1]).

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(crypto),
    [{started_applications, Started}| Config].


end_per_suite(Config0) ->
    {value, {_, Started}, Config} =
	lists:keytake(started_applications, 1, Config0),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    Config.


init_per_testcase(_TC, Config) -> Config.


end_per_testcase(_TC, Config) -> Config.


all() -> 
    [basic_split, basic_recover_static_short, basic_recover_static_long,
     basic_split_and_recover_short, basic_split_and_recover_long, cover].


-define(M, ekeyx_shamir).


basic_split(_Config) ->
    Secret = <<"Test">>,
    Shares = ?M:split_secret(3,5, Secret),
    5 = length(Shares),
    lists:foreach(fun(Share) ->
			  true = (size(Share) ==(size(Secret) + 1)) end,
		  Shares). 


basic_recover_static_short(_Config) ->
    Shares = lists:map(fun binary:decode_hex/1, [<<"0E4A">>, <<"9954">>]),
    <<"t">> = ?M:recover_secret(Shares).


basic_recover_static_long(_Config) ->
    Shares = lists:map(fun binary:decode_hex/1,
		       [<<"C8EF4C4201">>, <<"9673E6A402">>, <<"2AF9D99203">>,
			<<"992DC30904">>, <<"25A7FC3F05">>]),
    <<"test">> = ?M:recover_secret(Shares).


    
basic_split_and_recover_short(_Config) ->
    Secret = <<"t">>,
    Shares = ?M:split_secret(2, 2, Secret), 
    Secret = ?M:recover_secret(Shares).


basic_split_and_recover_long(_Config) ->
    Secret = <<"super secret">>,
    [S1, S2, S3, S4] = ?M:split_secret(2,4, Secret),
    Secret = ?M:recover_secret([S1, S2]),
    Secret = ?M:recover_secret([S1, S3]),
    Secret = ?M:recover_secret([S1, S4]),
    Secret = ?M:recover_secret([S2, S3]),
    Secret = ?M:recover_secret([S2, S4]),
    Secret = ?M:recover_secret([S3, S4]).


cover(_Config) ->
    {throw, "duplicate shares"} = 
	try ?M:recover_secret([<<"0E4A">>, <<"9954">>, <<"9954">>])
	catch C1:E1 -> {C1, E1} end,
    {throw, "shares must match in size"} = 
	try ?M:recover_secret([<<"0E">>, <<"9954">>])
	catch C2:E2 -> {C2, E2} end.
	
