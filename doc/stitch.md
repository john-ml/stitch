# Stitch

An expression-oriented language with strong type inference and
an emphasis on performance.

- (+1) Full type inference, even for recursive data types
- (+1) Generics and a simple trait system
- (+1) C's unboxed data and manual control over pointers
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
type string = *i8 # pointer type
type person = {age i32, height f32} # record type
type int_opt = <none unit, some int> # sum type / variant
```

Type aliases can be recursive and can take arguments:

```bash
type comparator(A) = (A, A) -> bool # function pointer type
type either(A, B) = <left A, right B>
type list(A) = <nil {}, cons {hd A, tl *list(A)}>
```

`@` is used to create values of sum types:

```bash
checked_div(a i32, b i32) <none {}, some i32> =
  if b == 0 then none@{} else some @ a/b
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

sum(xs *list(i32)) i32 =
  case *x {
    nil _ -> 0,
    some x -> x.hd + sum(x.tl)
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

### Looping

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
(It's a lot like a restricted `goto` that has to yield a value).

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

### Labels as values

(In short: Labels are basically 0-argument continuations that
don't require a closure.)

Labels can be passed as parameters to functions.
For example, the following
definition of `find` will return immediately once an item
is found rather than bubbling the value `true` all the way
back up the call stack:

```bash
find(x i32, xs *list(i32)) bool =
  # `..` delays the evaluation of the expression labelled `ret`
  find_helper(x, xs, ..ret: true)

# `success` is a label representing the suspended computation of a bool
find_helper(x, xs, success ..bool) =
  case *xs {
    nil _ -> false,
    cons xs ->
      if x == xs.ht then
        # Return to `ret` in the definition of find
        # As before, a label can only be evaluated in tail position
        ..success 
      else
        find_helper(x, xs, success)
  }
```

(Though, this is just a contrived example. `find` is simple enough
to be written as a normal loop:

```bash
find(x i32, xs *list(i32)) bool =
  rec:
    case *xs {
      nil _ -> false,
      cons xs -> x == xs.ht || (xs := xs.tl; rec)
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

Despite all this, labels aren't first class. The following use
of labels is not allowed:

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
choose[A](l1 ..A, l2 ..A) ..A = l1

f[A](ok ..A) ..A = choose(ok, ..not_ok: e)
```

`ok`'s lifetime is larger than that of `f`'s stack
frame, because `f` receives it as an argument. Since `choose`
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
  __eq__(v, w) =
    case *v {
      nil _ -> case *w {nil _ -> true, _ -> false},
      cons v -> case *w {
        nil _ -> false,
        cons w -> v.hd == w.hd && v.tl == w.tl
      }
    }
}
```

### Recursive types

Unlike ML/Haskell, Stitch allows recursive type aliases (and, in general,
supports equirecursive types rather than isorecursive ones). This
complicates type equality---for example, the following 5 list types are
considered equivalent, because they all have the same 'infinite expansions':

```bash
# Straightforward recursion
type list1(A) = <nil {}, cons {hd A, tl *list1(A)}>
# Straightforward recursion, but a different type alias
type list2(A) = <nil {}, cons {hd A, tl *list2(A)}>
# Recursion through a helper type
type opt(A) = <nil {}, cons A>
type list3(A) = opt({hd A, tl *list3(A)})
# Mutual bundle resulting in the same infinite expansion
type list4(A) = <nil {}, cons {hd A, tl *list4(A)}>
type list5(A) = <nil {}, cons {hd A, tl *list3(A)}>
```

In exchange, we gain the ability to write programs over recursive types
without ever explicitly declaring them. For example, the following program
evaluates simple expressions over boolean and integer values:

```bash
eval_int_exp(e) =
  case *e {
    lit i -> i,
    plus e -> eval_int_exp(e.a) + eval_int_exp(e.b),
    mult e -> eval_int_exp(e.a) * eval_int_exp(e.b),
    ifte e ->
      if eval_bool_exp(e.p) then
        eval_int_exp(e.a)
      else
        eval_int_exp(e.b)
  }

eval_bool_exp(e) =
  case *e {
    lit b -> b,
    and e -> eval_bool_exp(e.a) && eval_int_exp(e.b),
    not e -> !eval_bool_exp(e.a),
    leq e -> eval_int_exp(e.a) <= eval_int_exp(e.b)
  }
```

Inference will automatically produce the types of the ASTs being traversed
by these two functions:

```bash
type t1 = 
  *<lit i32,
    plus {a t1, b t1}, 
    mult {a t1, b t1}, 
    ifte {p t2, a t1, b t1}>

type t2 =
  *<lit bool,
    and {a t2, b t2},
    not {a t2},
    leq {a t1, b t1}>

eval_int_exp(e t1) i32 = ...

eval_bool_exp(e t2) bool = ...
```

...though, it will actually infer the following polymorphic signatures:

```bash
eval_int_exp[R1, R2, R3, R4, R5, R6](e t1(R1, R2, R3, R4, R5, R6)) i32 = ...

eval_bool_exp[R1, R2, R3, R4, R5, R6](e t2(R1, R2, R3, R4, R5, R6)) bool = ...
```

where the definitions of `t1` and `t2` are

```bash
type t1(R1, R2, R3, R4, R5, R6) = 
  *<lit i32,
    plus {a t1(R1, R2, R3, R4, R5, R6), b t1(R1, R2, R3, R4, R5, R6); R1}, 
    mult {a t1(R1, R2, R3, R4, R5, R6), b t1(R1, R2, R3, R4, R5, R6); R2}, 
    ifte {
      p t2(R1, R2, R3, R4, R5, R6),
      a t1(R1, R2, R3, R4, R5, R6),
      b t1(R1, R2, R3, R4, R5, R6);
      R3
    }>

type t2(R1, R2, R3, R4, R5, R6) =
  *<lit bool,
    and {a t2(R1, R2, R3, R4, R5, R6), b t2(R1, R2, R3, R4, R5, R6); R4},
    not {a t2(R1, R2, R3, R4, R5, R6); R5},
    leq {a t1(R1, R2, R3, R4, R5, R6), b t1(R1, R2, R3, R4, R5, R6); R6}>

```

### Memory

`dyn` is used to create a heap-allocated value:

```bash
type list(A) = *<nil {}, cons {hd A, tl list(A)}>

countdown(n i32) list(i32) =
  if n < 0 then
    dyn nil@{}
  else
    dyn cons @ {hd = n, tl = countdown(n - 1)}
```

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
    nil _ -> dyn nil@{}
    cons xs -> dyn cons @ {hd = 0, tl = zeros_like(xs.tl)}
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
  if (setjmp(lbl)) {
    [[e]] id
  } else {
    [[k lbl]] id
  }

-- Using labels
[[ ..lbl ]] _ = longjmp(lbl);
```

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

Potential complications:
- Mutable update
    - This analysis is still conservative:
      to typecheck `x as ?t1 := e as ?t2`, need `?t1 = ?t2` which
      will unify any labels in `?t2` that are trying to sneak out
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

