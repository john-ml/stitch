take z _ = nil.
take (s N) (X :: XS) = (X :: YS) <== take N XS = YS.

take (s (s (s (s z)))) (1 :: (2 :: (3 :: (4 :: (5 :: (6 :: (7 :: nil))))))) = XS?
