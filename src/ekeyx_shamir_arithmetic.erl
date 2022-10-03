-module(ekeyx_shamir_arithmetic).

-export([polynomial/2, evaluate/2, interpolate/3, aDD/2, mUL/2, dIV/2]).

-type polynomial() :: nonempty_list(non_neg_integer()).

-spec polynomial(non_neg_integer(), non_neg_integer()) -> polynomial().
polynomial(Intercept, Degree) ->
    [ Intercept | binary:bin_to_list(crypto:strong_rand_bytes(Degree))].


-spec evaluate(polynomial(), non_neg_integer()) -> non_neg_integer().
evaluate([R|_], X) when X == 0 -> R;
evaluate(Poly, X) -> 
    [ PolyTail | PolyRestRev ] = lists:reverse(Poly),
    lists:foldl(fun(PolyCoef, Acc) -> aDD(mUL(Acc, X), PolyCoef) end,
      PolyTail, PolyRestRev).


interpolate(Xsamples, Ysamples, X)
  when length(Xsamples) == length(Ysamples) ->
    Limit = length(Xsamples) - 1,
    AT = fun(L, I) -> lists:nth(I + 1, L) end,
    lists:foldl(
      fun(I, Result) ->
	      InnerRng = [V|| V <- lists:seq(0,Limit), V =/= I],
	      Basis = lists:foldl(
			fun(J, B) ->
				mUL(B, dIV(aDD(X, AT(Xsamples, J)),
					   aDD(AT(Xsamples, I), AT(Xsamples, J))))
			end,
			1, InnerRng),
	      Group = mUL(Basis, AT(Ysamples, I)),
	      aDD(Result, Group)
      end,
      0, lists:seq(0, Limit));
interpolate(_,_,_) -> throw(invalid_arguments).
    

dIV(_, 0) -> throw(badarith);
dIV(0, _) -> 0;
dIV(Lhs, Rhs) ->
    T = ekeyx_shamir_tables,
    T:exp((255 + (T:log(Lhs) - T:log(Rhs))) rem 255).


aDD(Lhs, Rhs) -> Lhs bxor Rhs.


mUL(0,_) -> 0;
mUL(_,0) -> 0;    
mUL(Lhs, Rhs) ->    
    T = ekeyx_shamir_tables,
    T:exp((T:log(Lhs) + T:log(Rhs)) rem 255).
