-module(ekeyx_shamir_arithmetic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([polynomial/1, evaluate/1, interpolate/1, op_add/1, op_mul/1, op_div/1]).

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
    [polynomial, evaluate, interpolate, op_add, op_mul, op_div, cover].

op_add(_Config) ->
    0 = ekeyx_shamir_arithmetic:op_add(16, 16),
    7 = ekeyx_shamir_arithmetic:op_add(3, 4).

op_mul(_Config) ->
    9 = ekeyx_shamir_arithmetic:op_mul(3, 7),
    0 = ekeyx_shamir_arithmetic:op_mul(3, 0),
    0 = ekeyx_shamir_arithmetic:op_mul(0, 3).

op_div(_Config) ->
    0 = ekeyx_shamir_arithmetic:op_div(0, 7),
    1 = ekeyx_shamir_arithmetic:op_div(3, 3),
    2 = ekeyx_shamir_arithmetic:op_div(6, 3).

polynomial(_Config) ->
    [42 | Rest] = ekeyx_shamir_arithmetic:polynomial(42, 2),
    2 = length(Rest).

evaluate(_Config) ->
    Poly = ekeyx_shamir_arithmetic:polynomial(42, 1),
    42 = ekeyx_shamir_arithmetic:evaluate(Poly, 0),
    ResPoly = ekeyx_shamir_arithmetic:evaluate(Poly, 1),
    ResExp = ekeyx_shamir_arithmetic:op_add(
        42, ekeyx_shamir_arithmetic:op_mul(1, lists:nth(2, Poly))
    ),
    ResPoly = ResExp.

interpolate(_Config) ->
    F = fun(I) ->
        Poly = ekeyx_shamir_arithmetic:polynomial(I, 2),
        Xvals = [1, 2, 3],
        Yvals = [ekeyx_shamir_arithmetic:evaluate(Poly, X) || X <- Xvals],
        ekeyx_shamir_arithmetic:interpolate(Xvals, Yvals, 0)
    end,
    L = lists:seq(0, 255),
    L = lists:map(F, L).

cover(_Config) ->
    {throw, badarith} =
        try
            ekeyx_shamir_arithmetic:op_div(1, 0)
        catch
            C1:E1 -> {C1, E1}
        end,
    {throw, invalid_arguments} =
        try
            ekeyx_shamir_arithmetic:interpolate([], [1], 5)
        catch
            C2:E2 -> {C2, E2}
        end.
