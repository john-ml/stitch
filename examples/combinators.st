-- Typed SK

s : ((R -> (A -> B)) -> ((R -> A) -> (R -> B))).
k : (A -> (_ -> A)).
b : ((B -> C) -> ((A -> B) -> (A -> C))).
c : ((A -> (B -> C)) -> (B -> (A -> C))).
(F $ X) : B <== F : (A -> B), X : A.

-- Deriving reader monad operations
-- PURE : (a -> (r -> a))?
-- JOIN : ((r -> (r -> a)) -> (r -> a))?
BIND : ((r -> a) -> ((a -> (r -> b)) -> (r -> b)))?

-- let k = const in let s = (<*>) in let c = flip in let b = (.) in
-- ((k $) ((((s $) ((c $) ((b $) k))) $) ((((((s $) b) $) c) $) ((((s $) b) $) k))))

-- let k = const in let s = (<*>) in let c = flip in let b = (.) in ((k $) ((k $) ((((((s $) s) $) k) $) ((((((b $) c) $) ((s $) c)) $) ((((c $) k) $) b)))))

-- Tuples

-- pair : (A -> (B -> (A * B))).
-- uncurry : ((A -> (B -> R)) -> ((A * B) -> R)).

-- Sums

-- inl : (A -> (A + _)).
-- inr : (B -> (_ + B)).
-- either : ((A -> R) -> ((B -> R) -> ((A + B) -> R))).

-- Lists

-- cons : (A -> ((list A) -> (list A))).
-- foldr : ((A -> (B -> B)) -> (B -> ((list A) -> B))).

-- Deriving list operations
-- ZIP ? ZIP : ((a * b) -> ((list a) -> ((list b) -> (list (a * b)))))

-- Deriving other combinators
-- 3 COMPOSE ? COMPOSE : ((b -> c) -> ((a -> b) -> (a -> c)))
-- 2 ID ? ID : (a -> a)
-- 5 BB ? BB : ((c -> d) -> ((a -> (b -> c)) -> (a -> (b -> d))))
-- 3 CURRY ? CURRY : (((a * b) -> c) -> (a -> (b -> c)))
-- 5 FLIP ? FLIP : ((a -> (b -> c)) -> (b -> (a -> c)))

-- Deriving cont monad operations
-- 3 FMAP ? FMAP : ((a -> b) -> (((a -> r) -> r) -> ((b -> r) -> r)))
-- 5 PURE ? PURE : (a -> ((a -> r) -> r))
-- 3 JOIN ? JOIN : (((((a -> r) -> r) -> r) -> r) -> ((a -> r) -> r))
-- 3 BIND ? BIND : (((a -> r) -> r) -> ((a -> ((b -> r) -> r)) -> ((b -> r) -> r)))

-- Deriving state monad operations
-- 5 FMAP ? FMAP : ((a -> b) -> ((s -> (a * s)) -> (s -> (b * s))))
-- 1 PURE ? PURE : (a -> (s -> (a * s)))
-- 3 JOIN ? JOIN : ((s -> (s -> (a * s))) -> (s -> (a * s)))
-- 5 BIND ? BIND : ((s -> (a * s)) -> ((a -> (s -> (b * s))) -> (s -> (b * s))))
