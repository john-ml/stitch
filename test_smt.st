fac {0} = {1}.
fac {N + 1} = {P} <== {N >= 0 /\ (N + 1) * K = P}, fac {N} = {K}.

cbrt {N * N * N} = N.

-- Works on both normal prolog clauses and formulas
all nil.
all (X :: XS) <== X, all XS.

-- Find N in [1..9] such that cbrt 27 = 2N - 5 and compute M := N!
all ({0 < N} :: ({N < 10} :: (cbrt {27} = {2*N - 5} :: nil))), fac {N} = {M}?
