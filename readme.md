# Stitch

A prolog with:
- Unification over cyclic data
- ~~Iterative deepening~~Tricks for complete search<sup>[1](https://github.com/johnli0135/stitch/blob/master/search-strategies.md)</sup>
- SMT solver for complicated constraints

[Examples.](https://github.com/johnli0135/stitch/blob/master/examples)

## Syntax

```hs
map _ nil = nil.
map F (X :: XS) = (Y :: YS) <==
  F X = Y,
  map F XS = YS.

z + N = N.
s N + M = (s P) <== N + M = P.

-- Find F such that map F [0, 1] = [1, 2]
map F (z :: (s z :: nil)) = (s z :: (s (s z) :: nil))?
-- F = (s z +)

-- Add a list of clauses to the goal
all nil.
all (X :: XS) <== X, all XS.
```

- `term ::= atom | Var | term term` instead of `term ::= atom | Var | atom(term, ..)`
    - Left assoc: `a b c d` => `((a b) c) d`
    - Allows for 'partial application'
    - Easier to write relations in standard notation
- `clause ::= term` instead of `clause ::= atom | atom(term, ..)`
- `<==` instead of `:-`

## Cycles

```hs
-- Some sugar for unifying stuff
X =:= X.

-- Unify two infinite lists: XS = [100, 100, ..], YS = [100, Z, 100, Z, ..]
XS =:= (100 :: XS),
YS =:= (100 :: (Z :: YS)),
XS =:= YS?
-- Z = 100

-- Unify two infinite lists defined in slightly different ways
XS =:= (100 :: (100 :: XS)),
YS =:= (100 :: (100 :: (100 :: ZS))),
ZS =:= (100 :: YS),
XS =:= YS?
-- yes
```

## Constraint solving

```hs
fac {0} = {1}.
fac {N + 1} = P <== {N >= 0 /\ (N + 1) * K = P}, fac {N} = {K}.

cbrt {N * N * N} = {N}.

-- Works on both normal prolog clauses and formulas
all nil.
all (X :: XS) <== X, all XS.

-- Find N in [1..9] such that cbrt 27 = 2N - 5
all ({0 < N} :: ({N < 10} :: (cbrt {27} = {2*N - 5} :: nil)))?
-- N = 4
```

- `{}` to write SMT expressions/formulas
- `{c}` as a clause adds constraint `c`
- Unifying terms `f {c1}` ~ `f {c2}` adds constraint `c1 = c2`
