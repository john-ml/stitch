# Stitch

An expression-oriented language with strong type inference and
an emphasis on performance.

- (+1) Full type inference, even for recursive data types
- (+1) Generics and a simple trait system
- (+1) C's unboxed data / control over indirection
- (+1) C's low runtime overhead (full specialization of traits & generics)
- (+1) C's compiler optimizations (we transpile to it)
- (-g<sub>64</sub>) C's memory safety

## Syntax and general overview

Function definitions look like ML:

```bash
double(x) = x + x

factorial(n) =
  if n <= 0 then
    1
  else
    n * factorial(n - 1)
```

There are two kinds of statements:
- `x = e` declares and initializes a new variable
- `x := e` mutates an existing one

As in ML, if `e` is an expression and `s` is a statement,
then `s; e` is an expression too:

```bash
triple_factorial(x) =
  res = factorial(x);
  res := factorial(res);
  res := factorial(res);
  res
```

Types can be manually specified:

```bash
triple_factorial(x i32) i32 =
  res as i32 = factorial(x);
  res := factorial(res);
  res := factorial(res);
  res
```

`type` defines a type alias:

```bash
type unit = {}
type integer = i32
type string = *i8 # pointer
type point = (f64, f64) # tuple
type person = {age i32, height f32} # record
type int_opt = <none unit, some int> # sum / variant
```

Type aliases can be recursive and can take arguments:

```bash
type comparator(A) = (A, A) -> bool # function pointer
type either(A, B) = <left A, right B>
type list(A) = *<nil (), cons (A, list(A))>
```

`@` is used to create values of sum types:

```bash
checked_div(a i32, b i32) <none (), some i32> =
  if b == 0 then none@() else some @ a/b
```

## Control flow

### Branching

Case expressions can be used to branch on sum types:

```bash
unwrap(x <none unit, some i32>, default i32) i32 =
  case x {
    none _ -> default,
    some i -> i
  }

sum(xs list(i32)) i32 =
  # Not full pattern matching: only allowed to branch on sum type tag and
  # destructure a tuple
  case *x {
    nil _ -> 0,
    some (hd, tl) -> hd + sum(tl)
  }
```

`when` can be used to branch on multiple conditional expressions:

```bash
categorize(i) =
  when {
    i < 0 -> "invalid input",
    i < 33 -> "small",
    i < 66 -> "medium",
    else -> "large"
  }
```

### Labelled expressions

(In short: Labels are basically 0-argument continuations that
don't require a closure.)

In place of looping constructs, expressions can be labelled
with `:` and can refer to other labels in scope with `..`:

```bash
mut_factorial(n i32) i32 =
  res = 1;
  rec: # The if-expression below is labelled `rec`
    if n == 1 then 
      res
    else
      res := res * n;
      n := n - 1;
      ..rec # Next iteration
```

Expressions can only refer to labels in tail position, and a label is
in scope only within the expression that it is labelling.

Special case: the short-circuiting operators `&&` and `||` desugar to
`if .. then .. else`s:

```bash
a && b -----> if a then b else a
a || b -----> if a then a else b
```

Thus, when passed as second argument
to these operators, labels can actually still be in tail position.
For example, the following is allowed:

```bash
arrays_equal(n u64, xs *i32, ys *i32) bool =
  i := 0;
  rec: i >= n || xs[i] == ys[i] && (i := i + 1; ..rec)
```

Labelled expressions can also be stored in local variables
representing suspended computations. For example, `goto fail`
style error handling could be implemented as:

```bash
f(x i32, default i32) i32 =
  y = acquire_resource(x);
  fail = ..lbl: (
    _ = print("Something went wrong!");
    _ = cleanup(y);
    default
  );
  if !condition1(x, y) then ..fail else
  z = action1();
  if !condition2(x, y, z) then ..fail else
  w = action2(y);
  if !condition3(x, y, z, w) then ..fail else
  res = action3(w);
  _ = cleanup(y);
  res
```

Labels can be passed as parameters to functions.
For example, the following
definition of `find` will return immediately once an item
is found rather than bubbling the value `true` all the way
back up the call stack:

```bash
find(x i32, xs list(i32)) bool =
  # `..` delays the evaluation of the expression labelled `ret`
  find_helper(x, xs, ..ret: true)

# `success` is a label representing the suspended computation of a bool
find_helper(x, xs, success ..bool) =
  case *xs {
    nil _ -> false,
    cons (h, t) ->
      if x == h then
        # Return to `ret` in the definition of find
        # As before, a label can only be evaluated in tail position
        ..success 
      else
        find_helper(x, t, success)
  }
```

(Though, this is just a contrived example. `find` is simple enough
to be written as a normal loop:

```bash
find(x i32, xs list(i32)) bool =
  rec:
    case *xs {
      nil _ -> false,
      cons (h, t) -> x == h || (xs := t; rec)
    }
```
)

Note:
- The type of the label need not be the same as the type of the value
  being currently computed (when you evaluate the label, control
  returns to a place where the label's type makes sense)
- Functions can take more than one label as argument and
  can be polymorphic over the label type
- Labels can be stored in data structures

As an example of all these:

```bash
categorize[A, B, C, D](
  i i32,
  s {small <left ..A, right ..A>, medium ..B},
  large ..C,
  fail ..D
) =
  when {
    i < 0 -> ..fail,
    i < 3 -> 
      case s.small {
        left k -> ..k,
        right k -> ..k
      },
    i < 6 -> ..s.medium,
    else -> ..s.large
  }
```

Despite all this, labels are subject to some restrictions.
The following use of labels is not allowed:

```bash
bad(i i32) ..i32 = ..not_ok: i * i
```

The label `not_ok` 'escapes upwards'---after `bad` returns,
`not_ok` contains information about a stack frame that no longer
exists. The compiler performs a conservative escape analysis and
only accepts programs in which it can prove that no label escapes
upwards.

This means rejecting some valid programs, like the following:

```bash
choose[A](p bool, l1 ..A, l2 ..A) ..A =
  if p then l1 else l2

f[A](ok ..A) ..A = choose(true, ok, ..not_ok: e)
```

`ok`'s lifetime is larger than that of `f`'s stack
frame, because `f` receives it as an argument. Since
the call to `choose`
always returns `ok`, this program will never crash at runtime.
Nonetheless, the escape analysis conservatively assumes
that `choose` might return `not_ok` (whose lifetime is smaller
than that of `f`'s stack frame) and so rejects `f`.

## Types

### Parametric polymorphism

Toplevel function definitions can be polymorphic.
Polymorphic type variables can be explicitly declared with `[]`:

```bash
id[A](x A) A = x

fst[A, B](p {x A, y B}) A = p.x
```

Polymorphic functions can be explicitly instantiated with `@[]`:

```bash
specific_fst(p {x i32, y f64}) i32 = fst@[i32, f64](p)
```

Restrictions compared to other languages:
- No polymorphic recursion
- No higher rank / higher kinded polymorphism

### Row/column polymorphism

Functions can be polymorphic over the 'rest of the fields in a record type'
(rows) or the 'rest of the options in a sum type' (columns).

Without manually annotating the previous example, the inference
algorithm would yield the following annotation for the definition of `fst`:

```bash
fst[A, R](p {x A; R}) A = p.x
```

`R` is a _row_ representing any other fields that might be in the input record `p`.
Thus `fst` can be called with any record that contains a field named `x`.

Dually, running the inference algorithm on

```bash
inl(x) = left@x
```

yields

```bash
inl[A, C](x A) <left A; C> = left@x
```

`C` is a _column_ representing any other options that could be in the return
value of `inl`.
Thus `inl` can return any sum type that contains the option `left`.

### Recursive types

Unlike ML/Haskell, Stitch allows recursive type aliases (and, in general,
supports equirecursive types rather than isorecursive ones). This
complicates type equality---for example, the following 5 list types are
considered equivalent, because they all have the same 'infinite expansions':

```bash
# Straightforward recursion
type list1(A) = *<nil {}, cons {hd A, tl list1(A)}>
# Straightforward recursion, but a different type alias
type list2(A) = *<nil {}, cons {hd A, tl list2(A)}>
# Recursion through a helper type
type opt(A) = *<nil {}, cons A>
type list3(A) = opt({hd A, tl list3(A)})
# Mutual bundle resulting in the same infinite expansion
type list4(A) = *<nil {}, cons {hd A, tl list4(A)}>
type list5(A) = *<nil {}, cons {hd A, tl list3(A)}>
```

In exchange, we gain the ability to write programs over recursive types
without ever explicitly declaring them. For example, the following program
evaluates simple expressions over boolean and integer values:

```bash
eval_int_exp(e) =
  case *e {
    lit i -> i,
    plus (a, b) -> eval_int_exp(a) + eval_int_exp(b),
    mult (a, b) -> eval_int_exp(a) * eval_int_exp(b),
    ifte (p, a, b) ->
      if eval_bool_exp(p) then
        eval_int_exp(a)
      else
        eval_int_exp(b)
  }

eval_bool_exp(e) =
  case *e {
    lit b -> b,
    and (a, b) -> eval_bool_exp(a) && eval_int_exp(b),
    not a -> !eval_bool_exp(a),
    leq (a, b) -> eval_int_exp(a) <= eval_int_exp(b)
  }
```

Inference will automatically produce the types of the ASTs being traversed
by these two functions:

```bash
type t1 = *<lit i32, plus (t1, t1), mult (t1, t1), ifte (t2, t1, t1)>

type t2 = *<lit bool, and (t2, t2), not t2, leq (t1, t1)>

eval_int_exp(e t1) i32 = ...

eval_bool_exp(e t2) bool = ...
```

### Traits

Traits are restricted compared to in other languages:
- Single parameter only
- No sub-/super-classing
- No polymorphic methods
- No higher kinded traits

They're mainly for operator overloading. Some examples:

```bash
# Equality (==)
trait A eq {__eq__ (A, A) -> bool}
# Inequality (<=)
trait A ord {__leq__ (A, A) -> bool}
# Truthiness
trait A bool {__bool__ A -> bool}
```

`bool` allows for overloading of `if .. then .. else`
(and, by extension, `&&` and `||`):

```bash
if p then a else b -----> if __bool__(p) then a else b
```

Instances can be declared with `impl`, and can depend on other instances:

```bash
# A linked list
type list(A) = *<nil {}, cons {hd A, tl list(A)}>

# If A is comparable, list(A) is too
impl(A eq) list(A) eq {
  __eq__(xs, ys) =
    case *xs {
      nil _ -> case *ys {nil _ -> true, _ -> false},
      cons (x, xs) -> case *ys {
        nil _ -> false,
        cons (y, ys) -> x == y && xs == ys
      }
    }
}
```

## Memory

Pointers are 'supposed to' hold the addresses of
valid memory locations only (e.g. not NULL), and dereferencing a
pointer that doesn't is undefined behavior.
To partially enforce this, the compiler proves the
following, using an escape analysis very similar to the one for labels:
- Pointers are never null
- Pointers to stack-allocated values always hold valid addresses

For example, `bad1` .. `bad4` are not allowed:

```bash
bad1() *i32 =
  x = 0;
  &x # x escapes

bad2() i32 =
  x = (
    y = 0;
    &y
  ); # y escapes the local scope...
  _ = use(x); # ...so use receives an invalidated pointer to y
  0

bad3(p **i32) i32 =
  x = 0;
  *p := &x; # x escapes into p
  0

f(p **i32, q *i32) i32 =
  *p := q;
  0

bad4(p **i32) i32 =
  x = 0;
  f(p, &x); # x escapes into p via f
  0
```

### Allocation / deallocation

`new` and `del` can be used to allocate and deallocate heap memory:

```bash
type list(A) = *<nil {}, cons {hd A, tl list(A)}>

countdown(n i32) list(i32) =
  if n < 0 then
    new nil@{}
  else
    new cons@{hd = n, tl = countdown(n - 1)}

del_list(del_elt (A) -> {}, l list(A)) =
  case *l {
    nil _ -> del l,
    cons l ->
      _ = del_elt(l.hd);
      _ = del_list(l.tl);
      del l
  }
```

If they were functions, their types would be:

```bash
new [A] A -> *A
del [A] *A -> {}
```

`new`'s type encodes the fact that heap-allocation
is the only way to comfortably allow pointers to escape
upwards: it's impossible to write a terminating
function of type `[A] A -> *A` without `new`.

These types also illustrate how weak the safety checks
for pointers are:
- If `*A` corresponds to heap-allocated memory, there's no
  guarantee that it holds a valid address and nothing forces
  you to delete it when no longer useful
- If `*A` corresponds to stack-allocated memory, nothing
  stops you from trying to delete it

### Defer

Though the typechecker won't help you delete responsibly,
`defer` tries to mitigate this by allowing you to write
a constructor and its corresponding destructor in
the same place; it will then automatically schedule the destructor
to be run when the constructed value falls out of scope.

Let `C [| e |]` represent an expression that contains 
the subexpression `e` within a surrounding context `C`.
Then, roughly, `C [| e defer g |]` becomes

```bash
tmp = e;
res = C [| tmp |];
_ = g(tmp);
res
```

and `C [| e defer x -> e1 |]` becomes

```bash
x = e;
res = C [| x |];
_ = e1;
res
```

For example,

```bash
del_noop(_) = {}

sum_countdowns(i, j, k) =
  sum(countdown(i) defer xs -> del_list(del_noop, xs))
  + sum(countdown(j) defer xs -> del_list(del_noop, xs))
  + sum(countdown(k) defer xs -> del_list(del_noop, xs))

sum(xs) =
  case *xs {
    nil _ -> 0,
    cons xs -> xs.hd + sum(xs.tl)
  }
```

becomes

```bash
sum_countdowns(i, j, k) =
  xs0 = countdown(i);
  xs1 = countdown(j);
  xs2 = countdown(k);
  res = (
    res = (
      res = sum(xs0) + sum(xs1) + sum(xs2)
      _ = del_list(del_noop, xs0);
      res
    );
    _ = del_list(del_noop, xs1);
    res
  );
  _ = del_list(del_noop, xs2);
  res
```

The same example, written another way:

```bash
del_alt(xs) = del_list(del_noop, xs)
sum_countdowns(i, j, k) =
  xs = countdown(i) defer del_alt;
  ys = countdown(j) defer del_alt;
  zs = countdown(k) defer del_alt;
  sum(xs) + sum(ys) + sum(zs)
```

becomes

```bash
sum_countdowns(i, j, k) =
  tmp0 = countdown(i);
  res = (
    xs = tmp0;
    tmp1 = countdown(j);
    res = (
      ys = tmp1;
      tmp2 = countdown(k);
      res = (
        zs = tmp2;
        sum(xs) + sum(ys) + sum(zs)
      );
      _ = del_alt(tmp2);
      res
    );
    _ = del_alt(tmp1);
    res
  );
  _ = del_alt(tmp0);
  res
```

Label invocation will also produce a slightly different translation:
let `C [| e |]` represent a sequence of statements that contains 
the subexpression `e` within a surrounding context `C`.
`C [| e defer x -> e1 |]; ..l` becomes

```
x = e;
C [| x |];
_ = e1;
..l
```

Since the label must be in tail position,
the deferred action `e1` has to happen before invoking the label.

For example,

```bash
sums(xs) =
  res = 0;
  rec:
    case *xs {
      nil _ -> res,
      cons xs ->
        res := res + sum(countdown(xs.hd) defer del_alt);
        xs = xs.tl;
        ..rec
    }
```

becomes

```bash
sums(xs) =
  res = 0;
  rec:
    case *xs {
      nil _ -> res,
      cons xs ->
        tmp = countdown(xs.hd);
        res := res + sum(tmp);
        xs = xs.tl;
        _ = del_alt(tmp);
        ..rec
    }
```

Things get a bit more complicated if the final expression
contains both branching and label invocations.
The deferred action has to be pushed down into each branch:

```bash
sums(p, xs) =
  res = 0;
  rec:
    case *xs {
      nil _ -> res,
      cons xs ->
        res := res + sum(countdown(xs.hd) defer del_alt);
        xs = xs.tl;
        if p then ..rec else 1
    }
```

becomes

```bash
sums(p, xs) =
  res = 0;
  rec:
    case *xs {
      nil _ -> res,
      cons xs ->
        tmp = countdown(xs.hd);
        res := res + sum(tmp);
        xs = xs.tl;
        if p then
          _ = del_alt(tmp);
          ..rec
        else
          _ = del_alt(tmp);
          1
    }
```

This allows `defer` to work nicely with `goto fail`-style error handling.
For example, the `goto fail`-style code from earlier can be rewritten as:

```bash
f(x i32, default i32) i32 =
  fail = ..lbl: (
    _ = print("Something went wrong!");
    default
  );
  y = acquire_resource(x) defer cleanup;
  if !condition1(x, y) then ..fail else
  z = action1();
  if !condition2(x, y, z) then ..fail else
  w = action2(y);
  if !condition3(x, y, z, w) then ..fail else
  action3(w)
```

The call to `cleanup` will be properly pushed into each `..fail` leaf
and `action3(w)` will become `res = action3(w); cleanup(y); res`,
as in the code without `defer`.

## Notes for implementation

### Inferring columns

During inference, the typechecker assumes that all `case` expressions
are exhaustive. Thus running the inference algorithm on

```bash
f(x) =
  case x {
    tag1 _ -> true, 
    tag2 _ -> false
  }
```

yields

```bash
f[A, B](x <tag1 A, tag2 B>) bool = ...
```

The algorithm can't infer a column type because that would imply the `case`
expression could be non-exhaustive.

If we add a final catch-all pattern, then it's safe to infer a column
type---it's possible for a `case` expression with a catch-all pattern
to be polymorphic in additional variant options while remaining exhaustive:

```bash
f[A, B, C](x <tag1 A, tag2 B; C>) bool =
  case x {
    tag1 _ -> true,
    tag2 _ -> false,
    _ -> true
  }
```

### Inferring recursive types

This might be doable using a modified version of HM inference where we omit the occurs check.
For example,

```bash
zeros_like(xs) =
  case *xs {
    nil _ -> new nil@{}
    cons xs -> new cons @ {hd = 0, tl = zeros_like(xs.tl)}
  }
```

yields the constraints

```bash
type T = *<nil ?A, cons {tl T; ?R1}>
type R = *<nil {}, cons {hd int, tl R}> # This should have failed occurs check
zeros_like(xs T) R = ...
```

where `A` and `R1` are metavariables.
Then, when generalizing, all type aliases get extra type parameters corresponding to
each metavariable:

```bash
type T(A, R1) = *<nil A, cons {tl T; R1}>
type R = *<nil {}, cons {hd int, tl R}>
zeros_like[A, R1](xs T(A, R1)) R = ...
```

Or, on a simplified version of expression evaluator,

```bash
eval_int_exp(e) =
  case *e {
    lit i -> i,
    shear e -> eval_int_exp(e.a) * eval_int_exp(e.b) + eval_int_exp(e.c),
    ifte e ->
      if eval_bool_exp(e.p) then
        eval_int_exp(e.a)
      else
        eval_int_exp(e.b)
  }

eval_bool_exp(e) =
  case *e {
    lit b -> b,
    nand e -> !(eval_bool_exp(e.a) && eval_int_exp(e.b)),
    leq e -> eval_int_exp(e.a) <= eval_int_exp(e.b)
  }
```

we get the constraints

```bash
type S =
  *<lit i32,
    shear {a S, b S, c S; ?R1},
    ifte {p T, a T, b T; ?R2}>

type T =
  *<lit bool,
    nand {a T, b T; ?R3},
    leq {a S, b S; ?R4}>

eval_int_exp(e S) i32 = ...

eval_bool_exp(e T) bool = ...
```

which, after closing over all metavariables in the definitions
for `S` and `T` and generalizing `eval_int_exp` and `eval_bool_exp`, yields

```bash
type S(R1, R2, R3, R4) =
  *<lit i32,
    shear {a S(R1, R2, R3, R4), b S(R1, R2, R3, R4), c S(R1, R2, R3, R4); R1},
    ifte {p T(R1, R2, R3, R4), a T(R1, R2, R3, R4), b T(R1, R2, R3, R4); R2}>

type T(R1, R2, R3, R4) =
  *<lit bool,
    nand {a T(R1, R2, R3, R4), b T(R1, R2, R3, R4); R3},
    leq {a S(R1, R2, R3, R4), b S(R1, R2, R3, R4); R4}>

eval_int_exp[R1, R2, R3, R4](e S(R1, R2, R3, R4)) i32 = ...

eval_bool_exp[R1, R2, R3, R4](e T(R1, R2, R3, R4)) bool = ...
```

Eliding occurs check doesn't break anything by itself. It allows bizarre things like:

```bash
type T(R) = T(R) -> R
omega[R](f T(R)) R = f(f)

bottom[A]() A = omega(omega)
```

but they can be ruled out by requiring that every recursive invocation of a type
alias be guarded by at least one pointer type constructor.

### Deciding equality between recursive types

Simplification: eliminate mutual recursion by repeatedly unfolding
aliases and, for now, ignore the fact that type aliases can take arguments and
that types will have metavariables in them.
Types are then

```
t \in type 
  = a         (atom)
  | T \vec t  (type constructor)
  | \mu v. t  (recursive type)
  | v         (back-pointer)
```

Syntactic equality up to renaming isn't enough:

- One type can be folded differently than another:

    ```bash
    \mu v. T(a, v) == T(a, \mu v. T(a, v))       # Unfolded outside \mu
                   == T(a, T(a, \mu v. T(a, v)))
                   == ...
                   == \mu v. T(a, T(a, v))       # Unfolded inside \mu
                   == \mu v. T(a, T(a, T(a, v)))
                   == ...

    \mu v. T(v, v) == T(\mu v. T(v, v), T(\mu v. T(v, v), \mu v. T(v, v))) # Both
    ```

- Different configurations of back-pointers can still refer to the same type:

    ```bash
    \mu v. T(v, v) == \mu v. T(\mu w. T(w, w), v)
    ```

Unification?

```hs
-- Idea: when checking \mu u. s ==? \mu v. t, make the assumption
-- u = v inside the subgoal s ==? t
-- Simultaneously keep track of the unfoldings [u -> s] and [v -> t]
unify : Env -> Type -> Type -> UnifyM ()

-- Back-pointers are only equal if they were assumed equal at some point
unify env u v = guard =<< liftA2 (==) (find u) (find v)

-- Atoms must match exactly
unify env a a = ret ()

-- Type constructors are equal if all arguments are
unify env (T \vec t1) (T \vec t2) = zipWithM_ (unify env) t1 t2

-- If we can make a new equality assumption / extend the environment,
-- do so; simultaneously unfold any back-pointers
unify env (\mu u. s) (\mu v. t) = do
  union u v
  unify env[u->s,v->t] (unify s t)
unify env u (\mu v. t) | env (\mu v. t) u = do
  union u v
  unify env[v->t] t (env u)
  
-- Even if we can't make any new assumptions / extend the environment,
-- unfold any back-pointers
unify env u t | env t u = unify env (env u) t

-- If the two types don't match even under all equality assumptions
-- and after unfolding back-pointers, they can't be equal 
unify _ _ _ = fail
```

Worst case: `unify empty (\mu u. T^p(u)) (\mu v. T^q(v))`
will take time proportional to lcm(p, q).

[An ancient text](http://cristal.inria.fr/~fpottier/publis/gauthier-fpottier-icfp04.pdf)
(citation 14) speaks of a linear time algorithm for deciding equality, but it's in French.

Desimplification 1: there are metavariables. Types are

```
t \in type 
  = a         (atom)
  | ?x        (metavariable)
  | T \vec t  (type constructor)
  | \mu v. t  (recursive type)
  | v         (back-pointer)
```

`unify` as defined above seems simple enough to extend:

```hs
unify env ?x t | t ?x =
  zonkWhnf ?x >>= \case
    ?x' -> unionMeta ?x t
    t' -> unify env t t'
```

Desimplification 2: type aliases take arguments. Types are

```
t \in type 
  = a                           (atom)
  | ?x                          (metavariable)
  | T \vec t                    (type constructor)
  | (\mu v(\vec A). t)(\vec t)  (recursive type alias with arguments \vec A,
                                 applied to types \vec t)
  | v                           (back-pointer)
```

During inference, there are two kinds of type aliases:
- 'open': produced by what should've been a failed occurs check, and not yet generalized.
  These could end up being type aliases that take arguments, but for now they are just
  `\mu v. t`s where `t` might have some metavariables.
- 'closed': type aliases that have already been closed over during generalization
  or user-defined recursive type aliases. These can take arguments.

Any time a closed type alias appears during inference, it'll be fully applied.
We can normalize all these type aliases so that all of their arguments are
fresh metavariables and give each full application a fresh new name:

```hs
norm : Type -> UnifyM NormalizedType
norm (\mu v(\vec A). t)(\vec t) = do
  ?xs <- freshMetas (length t)
  w <- freshAlias
  zipWithM_ union ?xs (\vec t)
  ret (\mu w. t[w / v][?xs1 / A1][?xs2 / A2]..)
```

Now all closed terms are open, and we can represent recursive types the old way,
as `\mu v. t`s.

The old unification algorithm wouldn't have worked because the `u = v` trick makes
too strong an assumption: given `(\mu u(\vec A). s)(\vec x) ==? (\mu v(\vec B). t)(\vec y)`,
assuming `u = v` would mean that the type aliases are equal for all applications, when we really just want to assume
that they are equal for the specific applications `\vec x` and `\vec y`.

Normalizing all type aliases solves this by assigning a unique alias name
for each specific application, and regular unification will take care of proving that
substituting `\vec x` and `\vec y` into `u` and `v` actually produces equal infinite expansions.

### Compiling labels

```hs
-- [[e]] k compiles an e with continuation k into C statements
[[_]] : Expr -> (Expr -> Expr) -> C.Stmts

-- Making labels
[[ ..lbl: e ]] k =
  jmp_buf lbl;
  volatile _x = x; -- for each x currently in scope and mutated in e or k
  if (setjmp(lbl)) {
    __x = _x; -- for each _x
    [[e]] id -- with __x replacing each x
  } else {
    __x = _x; -- for each _x
    [[k lbl]] id -- with __x replacing each x
  }

-- Using labels
[[ ..lbl ]] _ = longjmp(lbl);
```

[More info on `volatile`s](https://stackoverflow.com/questions/7996825/why-volatile-works-for-setjmp-longjmp).

For example,

```hs
f(x, y) = g(..l1: x + 1, ..l2: y + 1, ..l3: x*y + 1)
```

becomes

```C
int f(int x, int y) {
  jmp_buf l1;
  if (setjmp(l1)) {
    return x + 1;
  } else {
    jmp_buf l2;
    if (setjmp(l2)) {
      return y + 1;
    } else {
      jmp_buf l3;
      if (setjmp(l3)) {
        return x*y + 1;
      } else {
        return g(l1, l2, l3);
      }
    }
  }
}
```

### Label escape analysis

We might be able to trick the type checker into doing escape analysis.
In the surface language each label has type `..T` for some type `T`.
We can generate a unique metavariable for the type of each label, so that
labels now have type `..(?A, T)`. Inference / unification proceed normally.

Before generalization, we check if any metavariables
corresponding to labels defined locally within a function body show
up in its return type. (If so, those labels could escape upwards.)

We generalize the label metavariables too, so functions can be polymorphic
over them. Otherwise, the following would be rejected:

```bash
id_lbl[A](l: ..A) ..A = l
check_something(_, _) bool = ...

f(x, l) =
  _ = id_lbl(..l1: e);
  if check_something(x, l1) then
    l
  else
    id_lbl(l)
```

without a polymorphic `id_lbl`, its argument's label metavariable gets
unified with both `l` and `l1`:

```bash
id_lbl[A](l: ..(?L1, A)) ..(?L1, A) = l
check_something(_, _) bool = ...

f(x, l ..(?L1, A)) ..(?L1, A) =
  _ = id_lbl(..l1 as ..(?L1, A): e);
  if check_something(x, l1) then
    l
  else
    id_lbl(l)
```

since `l1`'s metavariable (`?L1`) appears in the return type of `f`,
it looks like `l1` could escape even though it can't.

Complication: mutable update. Labels could sneak out by mutating
arguments:

```bash
bad(p bool, q bool, l *..bool) i32 =
  *l := ..uhoh: p && q;
  0
```

`bad` puts `uhoh` into `l`, allowing the caller to compute
`&&` of two random locations in memory (or, more likely, two
random registers) using `..*l`.

Fix: the problem is that, with mutation, the return type isn't the only
way callees can yield values to callers. 
So instead of just checking the return type for label metavariables,
we have to check the argument types too.

This is what `bad` looks like right before generalization:

```bash
bad(p bool, q bool, l *..(?L, bool)) i32 =
  *l := ..uhoh as ..(?L, bool): p && q;
  0
```

Now the program is rejected, since the metavariable for `uhoh`
shows up in `l`'s type.

Complication: we can't just check arguments and return type, because
of local scoping.

```bash
bad() i32 =
  dangling = (x = e; &x);
  _ = use(dangling); # dangling can be used...
  0 # ...yet never escapes the entire function body
```

Fix: use the rules in "Choosing C when desugaring `defer`" to determine
scope. At scope end for each pointer, check if the `*` metavariable
shows up in the type of:
- The final expression
- The types of any variables in scope after scope end

Potential complications:
- User-defined type aliases
    - Label metavariables aren't in the surface language, so users
      can't talk about them
- Recursive type aliases containing labels
    - Label metavariables get closed over at generalization just 
      like everything else
    - Deciding type equality works the same too: the `..` type
      constructor just takes 2 arguments now instead of 1
- Trait methods that take in labels are now technically polymorphic
    - :(
- Labels and jumps nested inside each other
    - Within a single function body, scoping of label names prevents 
      referencing labels that don't yet exist
    - `..l: expression involving free labels l1 and l2` is safe because
      `l1` and `l2` must have larger lifetimes if you can talk about
      them inside `l`


### Compiling `*`

Should

```bash
bad(l *i32) *i32 = (tmp = l; l)
```

be rejected?

`tmp`'s metavariable will unify with `l`'s, so it may look like
`tmp` can escape.

On the other hand,

```bash
ok(l ..i32) ..i32 = (tmp = l; l)
```

is accepted, since temporary bindings aren't considered to be local
labels.

Is the same applicable to pointers? (Only pointers created
with the `&` operator are considered 'local'?)

```bash
ok1(l *i32) *i32 = (tmp = l; l)
ok2(l *i32) *i32 = (tmp = &*l; l)
bad(l *i32) *i32 = (tmp = &*l; tmp)
```

- In `ok1`, no 'local pointers' are created; `tmp` is a local variable
  that happens to bind a non-local one, which is fine.
- In `ok2`, pointer `&*l` is local, but its metavariable never
  unifies with anything and thus it can never escape.
- In `bad`, `tmp`'s metavariable unifies with the return type and thus
  could escape.

### Inference with pointer subtyping

?

### Choosing `C` when desugaring `defer`

`e defer _` will try to choose the smallest context `C` that contains
a statement `x = e'` or `x := e'` such that:
- `e` is a subexpression of `e'`
- `e'` is a simple expression (no `;`, `if`, `when`, `case`, `lbl:`, etc).

If this is not possible, `defer` will choose the largest simple
expression `e'` containing `e`.

e.g. in

```
a = e4;
x = (if p then y = (s; z = e3; e2); e1 else w);
z = e5;
e6
```

Suppose `e1 .. e5` are simple. Then:
- Anything in `e1`, `e2` or `e6` -> `C` is the empty context `[[ ]]`
- Anything in `e3` -> `C` is `z = [[_]]; e2`
- Anything in `e4` -> `C` is `a = [[_]]; x = ...; z = e5; r`
- Anything in `e5` -> `C` is `z = [[_]]; r`

```bash
cond_sum(p, i, j) =
  if p then
    sum(countdown(i) defer del_alt)
  else
    sum(countdown(j) defer del_alt)
```

becomes

```bash
cond_sum(p, i, j) =
  if p then
    tmp = countdown(i);
    res = sum(tmp);
    _ = del_alt(tmp);
    res
  else
    tmp = countdown(j);
    res = sum(tmp);
    _ = del_alt(tmp);
    res
```
