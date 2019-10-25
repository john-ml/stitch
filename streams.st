take z _ = nil.
take (s N) (X :: XS) = (X :: YS) <== take N XS = YS.

take (s (s (s (s z)))) (1 :: (2 :: (3 :: (4 :: (5 :: (6 :: (7 :: nil))))))) = XS?

X =:= X.

XS =:= (0 :: (1 :: XS)), take (s (s (s (s (s z))))) XS = YS?

map F nil = nil.
map F (X :: XS) = (Y :: YS) <== F X = Y, map F XS = YS.

succ Z = (s Z).

-- Doesn't seem to be possible even with iterative deepening
NATS =:= (z :: NATS1), map succ NATS = NATS1, take (s (s z)) NATS = XS?

-- map succ (z :: NATS1) = NATS1
--   NATS1 := s z :: YS.
--   succ z = (s z).
--   map succ (s z :: YS) = YS.
--     YS := s (s z) :: YS0.
--     succ (s z) = (s (s z)).
--     map succ (s (s z) :: YS0) = YS0.
--       ...
