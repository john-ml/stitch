# Stitch

A prolog with:
- Unification over cyclic data
- ~~Iterative deepening~~Tricks for complete search<sup>[1](#infinite-sadness)</sup>
- SMT solver for complicated constraints

Currently it
[works](https://github.com/johnli0135/stitch/blob/master/stitch.py),
but you have to write in abstract syntax.

NATS =:= (z :: NATS1), map succ NATS = NATS1, take (s (s z)) NATS = XS?

## Planned syntax

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
-- Some notation for binding temporaries
let X = X.

-- Unify two infinite lists: XS = [100, 100, ..], YS = [100, Z, 100, Z, ..]
let XS = (100 :: XS),
let YS = (100 :: (Z :: YS)),
let XS = YS?
-- Z = 100

-- Unify two infinite lists defined in slightly different ways
let XS = (100 :: (100 :: XS)),
let YS = (100 :: (100 :: (100 :: ZS))),
let ZS = (100 :: YS),
let XS = YS?
-- yes
```

## Constraint solving

```hs
fac 0 = 1.
fac {N + 1} = P <==
  {N >= 0 /\ (N + 1) * K = P},
  fac N = K.

cbrt {N * N * N} = N.

-- Works on both normal prolog clauses and formulas
all nil.
all (X :: XS) <== X, all XS.

-- Find N in [1..9] such that cbrt 27 = 2N - 5
all ({0 < N} :: ({N < 10} :: (cbrt 27 = {2*N - 5} :: nil)))?
-- N = 4
```

- `{}` to write SMT expressions/formulas
- `{c}` as a clause adds constraint `c`
- Unifying terms `f {c1}` ~ `f {c2}` adds constraint `c1 = c2`

---

<a name="infinite-sadness">1</a>: Just iterative deepening alone isn't enough
with cyclic structures. Here's a counterexample:

```hs
-- Some sugar for unifying things
X =:= X.

-- Extract a prefix from a (potentially infinite) list
take z _ = nil.
take (s N) (X :: XS) = (X :: YS) <== take N XS = YS.

map F nil = nil.
map F (X :: XS) = (Y :: YS) <== F X = Y, map F XS = YS.

-- nat constructor s : nat -> nat written as a 'function'
succ Z = (s Z).

-- Even with iterative deepening, the following query is problematic:
NATS =:= (z :: NATS1), map succ NATS = NATS1, take (s (s z)) NATS = XS?
```

The search tree arising from the second conjunct looks like

```hs
map succ (z :: NATS1) = NATS1
  NATS1 := s z :: YS.
  succ z = (s z).
  map succ (s z :: YS) = YS.
    YS := s (s z) :: YS0.
    succ (s z) = (s (s z)).
    map succ (s (s z) :: YS0) = YS0.
      ...
```

Iterative deepening pauses at increasing depths in this tree and
there's enough info to fully instantiate `XS` after a few rounds,
but the search needs to get to the 3rd conjunct (`take (s (s z)) NATS = XS`)
to actually make use of this info and it's never able to.

Reordering helps: if the query is actually
```hs
take (s (s z)) NATS = XS, NATS =:= (z :: NATS1), map succ NATS = NATS1?
```

then we get

```hs
take (s (s z)) NATS = XS
  NATS := X :: XS0
  XS := X :: YS
  take (s z) XS0 = YS
    XS0 := X0 :: XS1
    YS := X0 :: YS1
    take z XS1 = YS1
      YS1 := nil
-- So far: NATS := X :: (X0 :: XS1), XS := X :: (X0 :: nil).
NATS =:= (z :: NATS1)
  X := z
  NATS1 := X0 :: XS1
-- So far: NATS := z :: (X0 :: XS1), XS := z :: (X0 :: nil).
map succ NATS = NATS1
map succ (z :: (X0 :: XS1)) = (X0 :: XS1)
  succ z = X0
    X0 := s z
  -- So far: NATS := z :: (s z :: XS1), XS := z :: (s z :: nil).
  -- XS is ground!
  -- Continuing would just evaluate more and of NATS
  map succ (X0 :: XS1) = XS1
```

But it's still weird: the point where `XS` is ground isn't exactly a 'success':
there's still one subgoal remaining (`map succ (X0 :: XS1) = XS1`) and that subgoal
can never be cleared. So to get an `XS` out we have to (1) fiddle with clause order
and (2) consider hitting max depth to be some sort of 'soft success' where the repl
says 'not all subgoals are cleared and this could be completely inconsistent, but
here is what the instantiations look like'. Both are pretty annoying.

(1) could be fixed by literally brute-forcing all orderings e.g.

```py
go (P /\ Q /\ .., attempts=0)
  if attempts == factorial (len (P /\ Q /\ ..))
    fail
  try
    go P
    go (Q /\ ..)
  except HitMaxDepth
    go (next permutation of P /\ Q /\ .., attempts+1)
```

(2) doesn't seem like it can be fixed without a fancy rule for clearing
subgoals that produce infinite structures (something something coinduction).
