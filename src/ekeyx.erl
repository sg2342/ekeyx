-module(ekeyx).

-export([
    generate_shares/3,
    recover_secret/1,
    generate_shares/4,
    recover_secret/2
]).

-spec generate_shares(
    K :: pos_integer(),
    N :: pos_integer(),
    Secret :: nonempty_binary()
) ->
    Shares :: nonempty_list(binary()).
generate_shares(K, N, Secret) ->
    ekeyx_shamir:split_secret(K, N, Secret).

-spec generate_shares(
    K :: pos_integer(),
    N :: pos_integer(),
    Secret :: nonempty_binary(),
    Base :: binary | base16 | base64
) ->
    Shares :: nonempty_list(binary()).
generate_shares(K, N, Secret, Base) ->
    encode(ekeyx_shamir:split_secret(K, N, Secret), Base).

-spec recover_secret(Shares :: nonempty_list(binary())) ->
    Secret :: nonempty_binary().
recover_secret(Shares) -> ekeyx_shamir:recover_secret(Shares).

-spec recover_secret(
    Shares :: nonempty_list(binary()),
    Base :: binary | base16 | base64
) ->
    Secret :: nonempty_binary().
recover_secret(Shares, Base) ->
    ekeyx_shamir:recover_secret(decode(Shares, Base)).

encode(Shares, binary) ->
    Shares;
encode(Shares, base16) ->
    lists:map(
        fun string:lowercase/1,
        lists:map(fun binary:encode_hex/1, Shares)
    );
encode(Shares, base64) ->
    lists:map(fun base64:encode/1, Shares).

decode(Shares, binary) -> Shares;
decode(Shares, base16) -> lists:map(fun binary:decode_hex/1, Shares);
decode(Shares, base64) -> lists:map(fun base64:decode/1, Shares).
