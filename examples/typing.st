var x.
var y.

_ |- (0 : n).

_ :: (X : T) |- (X : T) <== var X.
-- G :: _ |- (X : T) <== G |- (X : T).

G |- (X Y : R) <== G |- (X : (T -> R)), G |- (Y : T).

-- G |- (x y : (a -> a))?
G |- (x 0 : a)?
