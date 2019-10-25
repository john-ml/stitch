-- Concatenate two lists
nil ++ XS = XS.
(X :: XS) ++ YS = (X :: ZS) <== XS ++ YS = ZS.

-- Sample query
(1 :: (2 :: nil)) ++ (3 :: (4 :: nil)) = XS?
(1 :: (2 :: nil)) ++ XS = (1 :: (2 :: (3 :: (4 :: nil))))?

-- Last element of a list
last XS = X <== INIT ++ (X :: nil) = XS.

-- last [1, 2, 3, 4]
last (1 :: (2 :: (3 :: (4 :: nil)))) = X?

