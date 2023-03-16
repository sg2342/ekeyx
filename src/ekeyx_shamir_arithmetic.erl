-module(ekeyx_shamir_arithmetic).

-export([polynomial/2, evaluate/2, interpolate/3, op_add/2, op_mul/2, op_div/2]).

-type polynomial() :: nonempty_list(non_neg_integer()).

-spec polynomial(non_neg_integer(), non_neg_integer()) -> polynomial().
polynomial(Intercept, Degree) ->
    [Intercept | binary:bin_to_list(crypto:strong_rand_bytes(Degree))].

-spec evaluate(polynomial(), non_neg_integer()) -> non_neg_integer().
evaluate([R | _], X) when X == 0 -> R;
evaluate(Poly, X) ->
    [PolyTail | PolyRestRev] = lists:reverse(Poly),
    lists:foldl(
        fun(PolyCoef, Acc) -> op_add(op_mul(Acc, X), PolyCoef) end,
        PolyTail,
        PolyRestRev
    ).

interpolate(Xsamples, Ysamples, X) when
    length(Xsamples) == length(Ysamples)
->
    Limit = length(Xsamples) - 1,
    AT = fun(L, I) -> lists:nth(I + 1, L) end,
    lists:foldl(
        fun(I, Result) ->
            InnerRng = [V || V <- lists:seq(0, Limit), V =/= I],
            Basis = lists:foldl(
                fun(J, B) ->
                    op_mul(
                        B,
                        op_div(
                            op_add(X, AT(Xsamples, J)),
                            op_add(AT(Xsamples, I), AT(Xsamples, J))
                        )
                    )
                end,
                1,
                InnerRng
            ),
            Group = op_mul(Basis, AT(Ysamples, I)),
            op_add(Result, Group)
        end,
        0,
        lists:seq(0, Limit)
    );
interpolate(_, _, _) ->
    throw(invalid_arguments).

op_div(_, 0) ->
    throw(badarith);
op_div(0, _) ->
    0;
op_div(Lhs, Rhs) ->
    ekeyx_shamir_tables:exp(
        (255 + (ekeyx_shamir_tables:log(Lhs) - ekeyx_shamir_tables:log(Rhs))) rem 255
    ).

op_add(Lhs, Rhs) -> Lhs bxor Rhs.

op_mul(0, _) ->
    0;
op_mul(_, 0) ->
    0;
op_mul(Lhs, Rhs) ->
    ekeyx_shamir_tables:exp((ekeyx_shamir_tables:log(Lhs) + ekeyx_shamir_tables:log(Rhs)) rem 255).
