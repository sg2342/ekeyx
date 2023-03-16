-module(ekeyx_shamir_arithmetic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([polynomial/1, evaluate/1, interpolate/1, aDD/1, mUL/1, dIV/1]).

-export([cover/1]).

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(public_key),
    [{started_applications, Started} | Config].

end_per_suite(Config0) ->
    {value, {_, Started}, Config} =
        lists:keytake(started_applications, 1, Config0),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    Config.

init_per_testcase(_TC, Config) -> Config.

end_per_testcase(_TC, Config) -> Config.

all() ->
    [polynomial, evaluate, interpolate, aDD, mUL, dIV, cover].

-define(M, ekeyx_shamir_arithmetic).

aDD(_Config) ->
    0 = ?M:aDD(16, 16),
    7 = ?M:aDD(3, 4).

mUL(_Config) ->
    9 = ?M:mUL(3, 7),
    0 = ?M:mUL(3, 0),
    0 = ?M:mUL(0, 3).

dIV(_Config) ->
    0 = ?M:dIV(0, 7),
    1 = ?M:dIV(3, 3),
    2 = ?M:dIV(6, 3).

polynomial(_Config) ->
    [42 | Rest] = ?M:polynomial(42, 2),
    2 = length(Rest).

evaluate(_Config) ->
    Poly = ?M:polynomial(42, 1),
    42 = ?M:evaluate(Poly, 0),
    ResPoly = ?M:evaluate(Poly, 1),
    ResExp = ?M:aDD(42, ?M:mUL(1, lists:nth(2, Poly))),
    ResPoly = ResExp.

interpolate(_Config) ->
    F = fun(I) ->
        Poly = ?M:polynomial(I, 2),
        Xvals = [1, 2, 3],
        Yvals = [?M:evaluate(Poly, X) || X <- Xvals],
        ?M:interpolate(Xvals, Yvals, 0)
    end,
    L = lists:seq(0, 255),
    L = lists:map(F, L).

cover(_Config) ->
    {throw, badarith} =
        try
            ?M:dIV(1, 0)
        catch
            C1:E1 -> {C1, E1}
        end,
    {throw, invalid_arguments} =
        try
            ?M:interpolate([], [1], 5)
        catch
            C2:E2 -> {C2, E2}
        end.
