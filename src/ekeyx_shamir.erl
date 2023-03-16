-module(ekeyx_shamir).

-export([split_secret/3, recover_secret/1]).

-define(A, ekeyx_shamir_arithmetic).

-spec split_secret(
    K :: pos_integer(),
    N :: pos_integer(),
    Secret :: nonempty_binary()
) ->
    Shares :: nonempty_list(binary()).
split_secret(K, N, Secret) when
    is_integer(N),
    is_integer(K),
    is_binary(Secret),
    size(Secret) > 0,
    N =< 255,
    K =< N,
    K >= 2
->
    % generate random X coordinates
    set_random_seed(),
    Xcoordinates = rand_shuffle(lists:seq(0, 254)),
    SharesInit = lists:duplicate(N, []),

    Zip = fun
        (L0, L1) when length(L0) < length(L1) ->
            lists:zip(L0, lists:sublist(L1, length(L0)));
        (L0, L1) when length(L0) >= length(L1) ->
            lists:zip(lists:sublist(L0, length(L1)), L1)
    end,
    Shares =
        lists:foldl(
            fun(Val, Shares) ->
                Poly = ?A:polynomial(Val, K - 1),
                lists:map(
                    fun({Xval, Yacc}) ->
                        X = ?A:aDD(Xval, 1),
                        Y = ?A:evaluate(Poly, X),
                        [Yacc, Y]
                    end,
                    Zip(Xcoordinates, Shares)
                )
            end,
            SharesInit,
            binary:bin_to_list(Secret)
        ),
    [
        binary:list_to_bin([Share, ?A:aDD(X, 1)])
     || {Share, X} <- Zip(Shares, Xcoordinates)
    ].

-spec recover_secret(Shares :: nonempty_list(binary())) ->
    Secret :: nonempty_binary().
recover_secret(Shares0) ->
    Shares = lists:map(fun binary:bin_to_list/1, Shares0),
    Sizes = [length(Share) || Share <- Shares],
    [Size | OtherSize] = lists:usort(Sizes),
    Ylen = Size - 1,
    XSamples = [lists:last(Share) || Share <- Shares],

    length(XSamples) == length(lists:usort(XSamples)) orelse
        throw("duplicate shares"),
    OtherSize == [] orelse
        throw("shares must match in size"),

    Res = lists:map(
        fun(Idx) ->
            YSamples = [lists:nth(Idx + 1, Share) || Share <- Shares],
            ?A:interpolate(XSamples, YSamples, 0)
        end,
        lists:seq(0, Ylen - 1)
    ),
    binary:list_to_bin(Res).

set_random_seed() ->
    %% https://hashrocket.com/blog/posts/the-adventures-of-generating-random-numbers-in-erlang-and-elixir
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> =
        crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}).

rand_shuffle(L0) ->
    {_, L} = lists:unzip(lists:sort([{rand:uniform(), V} || V <- L0])),
    L.
