# Search strategies

Just iterative deepening alone isn't enough for complete search with cyclic structures.
Here's a counterexample:

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

```
go (P /\ Q /\ .., attempts=0)
  if attempts == factorial (len (P /\ Q /\ ..))
    fail
  try
    go P
    go (Q /\ ..)
  except HitMaxDepth
    go (next permutation of P /\ Q /\ .., attempts+1)
```

a way better solution is probably to cycle between the conjuncts and let each have
a turn every once in a while (maybe prioritizing certain ones over others by
some heuristic about how much "progress" is made (e.g. a combination of number
of instantiations, branching factor, rate at which metavariables are being
introduced, etc.)).

(2) doesn't seem like it can be fixed without a fancy rule for clearing
subgoals that produce infinite structures (something something coinduction).
