-module(ekeyx).
-moduledoc """
Shamir's Secret Sharing (SSS) algorithm

byte-compatible to Hashicorp Vault's implementation of Shamir's Secret Sharing (SSS) algorithm.
""".

-export([
    generate_shares/3,
    recover_secret/1,
    generate_shares/4,
    recover_secret/2
]).

-doc """
Generate secret shares using Shamir's Secret Sharing algorithm.

## Parameters
   - `K`: number of shares required to recover the secret
   - `N`: number of total shares to generate
   - `Secret` : binary of the raw secret to split `N` ways, requiring `K` shares to recover.
   - `Base` : output encoding
""".
-spec generate_shares(
    K :: pos_integer(),
    N :: pos_integer(),
    Secret :: nonempty_binary(),
    Base :: binary | base16 | base64
) ->
    Shares :: nonempty_list(binary()).
generate_shares(K, N, Secret, Base) ->
    encode(ekeyx_shamir:split_secret(K, N, Secret), Base).

-doc """
Generate secret shares using Shamir's Secret Sharing algorithm.

Equivalent to
`generate_shares(K, N, Secret, binary)`
""".
-spec generate_shares(
    K :: pos_integer(),
    N :: pos_integer(),
    Secret :: nonempty_binary()
) ->
    Shares :: nonempty_list(binary()).
generate_shares(K, N, Secret) ->
    ekeyx_shamir:split_secret(K, N, Secret).

-doc """
Recover secret from shares.

Number of `Shares` must be equal or greater than the `K` parameter used to generate shares.
`Base` sets output encoding.
""".
-spec recover_secret(
    Shares :: nonempty_list(binary()),
    Base :: binary | base16 | base64
) ->
    Secret :: nonempty_binary().
recover_secret(Shares, Base) ->
    ekeyx_shamir:recover_secret(decode(Shares, Base)).

-doc """
Recover secret from shares.

Equivalent to
`recover_secret(Shares, binary)`
""".
-spec recover_secret(Shares :: nonempty_list(binary())) ->
    Secret :: nonempty_binary().
recover_secret(Shares) -> ekeyx_shamir:recover_secret(Shares).

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
