# Stitch

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
type int_opt = None | Some(int) # sum / variant
```

Type aliases can be recursive and can take arguments:

```bash
type comparator(a) = (a, a) -> bool # function pointer
type either(a, b) = Left(a) | Right(b)
type list(a) = *(Nil | Cons(a, list(a)))
```

Only sum type constructors can start with capital letters.
Constructing values of sum types looks like a function call:

```bash
checked_div(a i32, b i32) None | Some i32 =
  if b == 0 then None else Some(a / b)
```

## Control flow

### Branching

Case expressions:

```bash
type option(a) = None | Some(a)
unwrap(x option(i32), default i32) i32 =
  case x {
    None -> default,
    Some(i) -> i
  }

sum(xs list(i32)) i32 =
  # Not full pattern matching: only allowed to branch on sum type tag and
  # destructure a tuple
  case *x {
    Nil -> 0,
    Cons(hd, tl) -> hd + sum(tl)
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

The `else` clause is required.

### Labels

Expressions can be labelled, allowing for iteration:

```bash
mut_factorial(n i32) i32 =
  res = 1;
  rec: # The if-expression below is labelled `rec`
    if n == 1 then 
      res
    else
      res := res * n;
      n := n - 1;
      rec # Next iteration
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
  i = 0;
  rec: i >= n || xs[i] == ys[i] && (i := i + 1; rec)
```

## Types

### Parametric polymorphism

Toplevel function definitions can be polymorphic.
Polymorphic type variables can be explicitly declared with `[]`:

```bash
id[a](x a) a = x

fst[a, b](p {x a, y b}) a = p.x
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
fst[a, r](p {x a; r}) a = p.x
```

`r` is a _row_ representing any other fields that might be in the input record `p`.
Thus `fst` can be called with any record that contains a field named `x`.

Dually, running the inference algorithm on

```bash
inl(x) = Left(x)
```

yields

```bash
inl[a, c](x a) (Left(a) + c) = Left(x)
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
type list1(a) = *(Nil | Cons({hd a, tl list1(a)}))
# Straightforward recursion, but a different type alias
type list2(a) = *(Nil | Cons({hd a, tl list2(a)}))
# Recursion through a helper type
type opt(a) = *(Nil | Cons(a))
type list3(a) = opt({hd a, tl list3(a)})
# Mutual bundle resulting in the same infinite expansion
type list4(a) = *(Nil | Cons({hd a, tl list5(a)}))
type list5(a) = *(Nil | Cons({hd a, tl list4(a)}))
```

In exchange, we gain the ability to write programs over recursive types
without ever explicitly declaring them. For example, the following program
evaluates simple expressions over boolean and integer values:

```bash
eval_int_exp(e) =
  case *e {
    Lit(i) -> i,
    Plus(a, b) -> eval_int_exp(a) + eval_int_exp(b),
    Mult(a, b) -> eval_int_exp(a) * eval_int_exp(b),
    Ifte(p, a, b) ->
      if eval_bool_exp(p) then
        eval_int_exp(a)
      else
        eval_int_exp(b)
  }

eval_bool_exp(e) =
  case *e {
    Lit(b) -> b,
    And(a, b) -> eval_bool_exp(a) && eval_int_exp(b),
    Not(a) -> !eval_bool_exp(a),
    Leq(a, b) -> eval_int_exp(a) <= eval_int_exp(b)
  }
```

Inference will automatically produce the types of the ASTs being traversed
by these two functions:

```bash
type t1 = *
  ( Lit(i32)
  | Plus(t1, t1)
  | Mult(t1, t1)
  | Ifte(t2, t1, t1)
  )

type t2 = *
  ( Lit(bool)
  | Plus(t2, t2)
  | Not(t2)
  | Leq(t1, t1)
  )

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
trait a eq {__eq__ (a, a) -> bool}
# Inequality (<=)
trait a ord {__leq__ (a, a) -> bool}
# Truthiness
trait a bool {__bool__ a -> bool}
```

`bool` allows for overloading of `if .. then .. else`
(and, by extension, `&&` and `||`):

```bash
if p then a else b -----> if __bool__(p) then a else b
```

Instances can be declared with `impl`, and can depend on other instances:

```bash
# A linked list
type list(a) = *(Nil | Cons({hd a, tl list(a)}))

# If a is comparable, list(a) is too
impl[a](a eq) list(a) eq {
  __eq__(xs, ys) =
    case *xs {
      Nil -> case *ys {Nil -> True, _ -> False},
      Cons(x, xs) -> case *ys {
        Nil -> False,
        Cons(y, ys) -> x == y && xs == ys
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

### Allocation

`new` can be used to allocate heap memory:

```bash
type list(a) = *(Nil | Cons({hd a, tl list(a)}))

countdown(n i32) list(i32) =
  if n < 0 then
    new Nil
  else
    new Cons({hd = n, tl = countdown(n - 1)})
```

If it was a function, `new`'s type would be:

```bash
new [a] a -> *a
```

`new`'s type encodes the fact that heap-allocation
is the only way to comfortably allow pointers to escape
upwards: it's impossible to write a terminating
function of type `[a] a -> *a` without `new`.

## Notes for implementation

### Inferring columns

During inference, the typechecker assumes that all `case` expressions
are exhaustive. Thus running the inference algorithm on

```bash
f(x) =
  case x {
    Tag1 _ -> True, 
    Tag2 _ -> False
  }
```

yields

```bash
f[a, b](x (Tag1 a | Tag2 b)) bool = ...
```

The algorithm can't infer a column type because that would imply the `case`
expression could be non-exhaustive.

If we add a final catch-all pattern, then it's safe to infer a column
type---it's possible for a `case` expression with a catch-all pattern
to be polymorphic in additional variant options while remaining exhaustive:

```bash
f[a, b, c](x (Tag1 a | Tag2 b + c)) bool =
  case x {
    Tag1 _ -> True,
    Tag2 _ -> False,
    _ -> True
  }
```

### Inferring recursive types

This might be doable using a modified version of HM inference where we omit the occurs check.
For example,

```bash
zeros_like(xs) =
  case *xs {
    Nil -> new Nil
    Cons(xs) -> new Cons({hd = 0, tl = zeros_like(xs.tl)})
  }
```

yields the constraints

```bash
type t = *(Nil, Cons({tl t; ?r1}))
type r = *(Nil, Cons({hd int, tl r})) # this should have failed occurs check
zeros_like(xs t) r = ...
```

where `r1` is a metavariable.
Then, when generalizing, all type aliases get extra type parameters corresponding to
each metavariable:

```bash
type t(r1) = *(Nil | Cons({tl t; r1}))
type r = *(Nil | Cons({hd int, tl r}))
zeros_like[r1](xs t(r1)) r = ...
```

Or, on a simplified version of expression evaluator,

```bash
eval_int_exp(e) =
  case *e {
    Lit(i) -> i,
    Arith(e) -> eval_int_exp(e.a) * eval_int_exp(e.b) + eval_int_exp(e.c),
    Ifte(e) ->
      if eval_bool_exp(e.p) then
        eval_int_exp(e.a)
      else
        eval_int_exp(e.b)
  }

eval_bool_exp(e) =
  case *e {
    Lit(b) -> b,
    Nand(e) -> !(eval_bool_exp(e.a) && eval_int_exp(e.b)),
    Leq(e) -> eval_int_exp(e.a) <= eval_int_exp(e.b)
  }
```

we get the constraints

```bash
type s = *
  ( Lit(i32)
  | Arith({a s, b s, c s; ?r1})
  | Ifte({p t, a t, b t; ?r2})
  )

type t = *
  ( Lit(bool)
  | Nand({a t, b t; ?r3})
  | Leq({a s, b s; ?r4})
  )

eval_int_exp(e s) i32 = ...

eval_bool_exp(e t) bool = ...
```

which, after closing over all metavariables in the definitions
for `s` and `t` and generalizing `eval_int_exp` and `eval_bool_exp`, yields

```bash
type s(r1, r2, r3, r4) = *
  ( Lit(i32)
  | Arith({a s(r1, r2, r3, r4), b s(r1, r2, r3, r4), c s(r1, r2, r3, r4); r1})
  | Ifte({p t(r1, r2, r3, r4), a t(r1, r2, r3, r4), b t(r1, r2, r3, r4); r2})
  )

type t(r1, r2, r3, r4) = *
  ( Lit(bool)
  | Nand({a t(r1, r2, r3, r4), b t(r1, r2, r3, r4); r3})
  | Leq({a s(r1, r2, r3, r4), b s(r1, r2, r3, r4); r4})
  )

eval_int_exp[r1, r2, r3, r4](e s(r1, r2, r3, r4)) i32 = ...

eval_bool_exp[r1, r2, r3, r4](e t(r1, r2, r3, r4)) bool = ...
```

Eliding occurs check doesn't break anything by itself. It allows bizarre things like:

```bash
type T(r) = T(r) -> r
omega[r](f T(r)) r = f(f)

bottom[a]() a = omega(omega)
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

### Label escape analysis

NOTE: Used to have 'labels as values' (0-argument continuations). Same tricks
apply for escape analysis on pointers and closures.

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
  _ = id_lbl(l1: e);
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
  _ = id_lbl(l1 as ..(?L1, A): e);
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
  *l := uhoh: p && q;
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
  *l := uhoh as ..(?L, bool): p && q;
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

At scope end for each pointer, check if the `*` metavariable
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

### Generalization

After typechecking some function body `e`, context will contain:
- Equality constraints `?x ~ ?y` among metas
- (Possibly recursive) instantiations for metas `?x -> t`
- Constraints of the form `trait1 -> t, ..; ..`

After generalization:
- Each uninstantiated meta becomes a generic type variable
- Each instantiated meta yields a type alias which closes over any
  free variables in its instantiation, and uses of the instantiated
  meta become application of that type alias
- Constraints on non-type-variables must be solved (by running the `impl`
  declarations as a restricted Prolog program) until all constraints are on
  type variables only

### Typechecking functions

```bash
f[a tr1 .., ..](x t1, ..) r = e
```

- What happens if infer stronger constraints than given in `[]`?
  Implicitly adding those constraints is not ok, because then
  type annotations can't be upper bounds
- Use `_`?
    - If `f[a]` then turn off generalization (assume the annotation
      is exhaustive)
    - If `f[a, _]` then use the annotation but still generalize
      other type variables afterwards
    - If `f[a tr1, _]` then keep generalization on but after
      constraint solving, `tr1` must be the only constraint on `a`
    - If `f[a tr1 _, _]`, then after constraint solving, the traits for `a`
      must contain `tr1`


### Deallocation

At every scope end, some pointers become inaccessible. Any inaccessible
pointers returned by `new` need to be `del`eted.

The escape analysis for `&` proves that certain pointers (and thus everything
that they point to) are inaccessible at scope end. These can be recursively
`del`eted.

e.g. 

```bash
f() =
  xs as list(i32) = countdown(i);
  0
  # xs is dead

countdown(i) list(i32) =
  if i == 0 then new Nil else new Cons(i, countdown(i - 1))
```

But the analysis sometimes says things are accessible when they actually
aren't:

```bash
f(p bool, ys list(i32)) =
  xs = new Cons(1, ys);
  ys
  # xs 100% doesn't escape, but analysis says it might
```

and sometimes it's only half right:

```bash
f(p bool, ys list(i32)) =
  xs = countdown(100);
  if p then ys else xs
  # xs may or may not escape depending on runtime value `p`

g(p bool, ys list(i32)) =
  xs = countdown(100);
  append(ys, drop(5, xs))
  # Only part of xs actually escapes. The first 5 cons cells are inaccessible
  # after this function returns

drop(n, xs) =
  if n == 0 then xs else case *xs {
    Nil -> xs
    Cons(_, xs) -> drop(n - 1, xs)
  }

append(xs, ys) =
  case *xs {
    Nil -> ys
    Cons(x, xs) -> new Cons(x, append(xs, ys))
  }
```

So we need to do some runtime checks to figure out which parts of `xs` are actually
accessible and free the inaccessible stuff.

One way to do that:
1. Traverse every variable that will persist after scope end and might
   be aliased inside `xs` and mark each pointer as 'accessible' along the way.
2. Traverse `xs` until you hit a marked pointer. Along the way, `del` unmarked
   `new`'d pointers.
3. Traverse every variable from (1) again and clear each 'accessible' mark.

So, in `f`, `meta(xs) ~ meta(ys)` and the analysis thinks that `xs` could contain
aliases to `ys` (which it can't) or to the return value (which it can).
1. `ys` is traversed in full, marking each pointer as 'accessible'. The return
   value is also traversed: if `p`, then this traversal ends immediately since
   the return value is just `ys` and thus already marked; otherwise, all of `xs`
   is marked as accessible.
2. If `p`, then `xs` is traversed in full and every pointer is freed. Otherwise,
   `xs` immediately encounters an 'accessible' pointer and nothing is freed.
3. `ys` is traversed in full and each mark is cleared. If not `p`, then all of `xs`
   is traversed and cleared too.

In `g`, something cool happens: with explicit pointer metas, `append` has
type

```bash
append[p, q](xs list(p, i32), ys list(q, i32)) list(q, i32) = ...
```

So the compiler is able to prove that `xs` and `drop(5, xs)` never alias `ys`.
1. The return value `append(ys, drop(5, xs))` is traced in full and that's it.
   (Don't need to trace `ys`, because we know it can't alias!)
2. Two intermediate values are possibly dead after this function returns: `xs`
   and `drop(5, xs)`. Let's just say `xs` is traversed first: the first 5 cons
   cells are unmarked, so they get freed. The 6th cons cell is marked so
   the traversal stops immediately and nothing else is freed. `drop(5, xs)`'s
   traversal stops immediately and nothing is freed.
3. All marks in `append(ys, drop(5, xs))` are cleared.

That's potentially a lot of scans at each scope end and space is needed to
store the marks (you could stuff the mark bits inside pointers, but it's a bit
weird: to check if you've seen *p = (*q, *r) before, you need to check if either
bits q or r is marked, not p). Marking means that no part
of the graph is scanned more than once (so the more sharing the better), but this
still sucks.

Only recursive types can have back-pointers, so you only need to mark/unmark
the roots of recursive types (lists, trees, etc) while tracing. This probably
won't help much since recursive types are basically all that pointers are
used for.

We can delay tracing until the next scope end:
- Values that would need to be traced at current scope end might themselves be
  eligible for deletion (e.g. if x only depends on y and at next scope end, y
  doesn't depend on anything and y is eligible for deletion, then we can just
  delete both x and y without tracing anything. This probably ends up being
  similar to Tofte-Taplin regions?)

Delay tracing ==> ok time, bad space
Eager tracing ==> bad time, ok space

But: delay tracing 100% => ok time; delay tracing a little bit could be worse
if erase pointer metas (may trace more than necessary).

Let `l` be the set of pointers reachable from relevant live values and `d`
be the set of pointers reachable from dying values.

Is pass 3 completely redundant? Right now each pointer must be associated with
2 additional pieces of information:
- live/dead
- visited/unvisited

3-pass algo is:
1. Traverse l. Stop if visited. Set visited.
2. Traverse d. Stop if dead or visited. Set dead.
3. Traverse l. Stop if unvisited. Set unvisited.

aka
1. Set l all visited.
2. Set d - l all dead.
3. Set l all unvisited.

Can we alternate between two 2-pass algos?

Algo 1:
1. Traverse l. Stop if visited. Set visited.
2. Traverse d. Stop if dead or visited. Set dead.

Without pass 3, things in l can never die because nothing sets them unvisited.
For this to work, the 2nd 2-pass algo has to set some things as unvisited.
After algo 1, visited is partially wrong but dead is correct. So the 1st pass
can trust the info in dead. The only way to make use of this is to mark things
as dead:

1. Traverse d. Stop if dead. Set dead.

Visited is only wrong when something currently dying (i.e. in d) has been set
visited. Since we're traversing the things that are currently dying, we can
right all of these wrongs by marking things unvisited as we go:

1. Traverse d. Stop if dead. Set dead and unvisited.

Now dead is wrong and needs to be fixed. We have to traverse l and set things
live.

2. Traverse l. ???. Set live.

We need to detect cycles somehow. We could mark things as visited as we go...

2. Traverse l. Stop if visited. Set visited and live.

So we have

Algo 1:
1. Traverse l. Stop if visited. Set visited.
2. Traverse d. Stop if dead or visited. Set dead.

Algo 2:
1. Traverse d. Stop if dead. Set dead and unvisited.
2. Traverse l. Stop if visited. Set visited and live.

This doesn't work:

{
  xs = [1, 2, 3]
  {
    {
      ys = 4 : tail xs
    } # visited = {1, 2, 3}, dead = 4
    zs = 5 : tail xs
  } # visited = 1, dead = 2, 3, 5
  # Uh oh, {2, 3} gone
}

Visited isn't a correct cycle-breaker: there could be more things to mark live
even if the current pointer is marked visited.

1. Traverse d. Stop if dead. Set dead.
2. Traverse l. Stop if visited. Set live and visited.

{
  xs = [1, 2, 3]
  {
    {
      zs = 4 : tail xs
    } # dead = 4, visited = {1, 2, 3}
    ys = 5 : tail xs
  } # dead = {5, 2, 3}, visited = {1, 2, 3}
  # Uh oh, {2, 3} gone
}

1. Traverse d. Stop if dead. Set dead and unvisited.
2. Traverse l. Stop if visited. Set live and visited.

{
  xs = [1, 2, 3]
  {
    {
      zs = 4 : tail xs
    } # dead = 4, visited = {1, 2, 3}
    ys = 5 : tail xs
  } # dead = {5, 2, 3}, visited = {1, 2, 3}
  # Uh oh, {2, 3} gone
}

1. Traverse l. Stop if visited. Set visited.
2. Traverse d. Stop if visited or dead. Set dead.
3. Traverse l. Stop if unvisited. Set unvisited.

We don't need the extra bitmap:

1. Traverse l. Stop if dead. Set dead.
2. Traverse d. Stop if dead. Set dead.
3. Traverse l. Stop if live. Set live.

But for now let's use it: `mark(p)` for p in l, `trim(p)` for
p in d, and `unmark(p)` for p in l.

Also, the 'alias analysis' isn't as crude as I thought:

```bash
f(p, xs) =
  ys = ...;
  if p then xs else ys
```

during typechecking, can elaborate into

```bash
f(p, xs) =
  ys = ...;
  if p then (
    res = xs;
    trim(ys);
    res
  ) else (
    res = ys;
    mark(res);
    trim(ys);
    unmark(res);
    res
  )
```

the metas for `xs` and `ys` will eventually unify, but each case arm
is checked before that. So at elaboration time, the arms don't influence
each other. In then-branch, ys never aliases xs and can be trimd; in
else-branch, ys shows up in the result and so the result has to be traced.

How can we save on marks? Instead of bitmaps, we can have a header for each
allocation tracking its 'nesting depth' in the execution trace. Maybe this
would help avoid retracing stuff?

Let `L(p)` be the nesting depth of some pointer `p`.

Let `k` be the nesting depth of the current vertex in the traversal.

1. mark(p, L(p)) for p in sort l on L: Stop if k <= L(p). Set L(p).
2. trim(p, n) for p in d: Stop if k <= n or dead. Set dead.

Then the above example becomes

```bash
# let d be the depth on entry into this function
# d
f(p, xs) =
  # d + 1
  ys = ...;
  if p then
    # d + 2
    res = xs;
    # End d + 2, now at d + 1: l = d = empty
    # End d + 1, now at d: l = empty, d = ys
    # 1: nothing to mark
    # 2: trim ys
    trim(ys, d);
    res
  else
    # d + 2
    res = ys;
    # d + 1: l = d = empty
    # d: l = res, d = ys
    # 1: mark res
    mark(res, d);
    # 2: trim ys
    trim(ys, d);
    res
```

So we avoid two passes over `res` but 
recursively escaping stuff still quadratic:

```bash
f(n) = if n == 0 then new Nil else new Cons(n, f(n - 1))
```

becomes

```bash
f(n) =
  if n == 0 then
    res = new Nil;
    res
  else
    xs = f(n - 1);
    res = new Cons(n, xs);
    # d: l = res, d = xs
    mark(res, d);
    trim(xs, d);
    res
```

`trim` will always immediately return, but `mark` will go through the whole list
every time.

Options:
- Don't mark+trim at every single scope end: keep a queue of things to be
  trimmed. Whenever `new` 'runs out of memory', mark everything in scope
  and trim the entire queue at once. Like gc, but don't need to mark
  every root (just the ones currently in scope) and don't need to traverse
  entire heap to figure out what to free/preserve.
- Somehow initialize every heap pointer with the correct nesting level, so
  marking is never necessary.

Magic initialization:

```bash
f(n) = if n == 0 then new Nil else new Cons(n, f(n - 1))
```

becomes

```bash
f(d, n) = if n == 0 then new d Nil else new d Cons(n, f(d, n - 1))
```

The extra parameter `d` represents the nesting depth. Then just need trim:

```bash
f(d, n) =
  if n == 0 then
    res = new d Nil;
    res
  else
    xs = f(d, n - 1);
    res = new d Cons(n, xs);
    trim(xs, d);
    res
```

...probably can't perfectly initialize and get rid of marking entirely.
But can try to underestimate the nesting depth so that marking has a chance
to cutoff earlier.

```bash
f(d, n) =
  if n == 0 then
    res = new d Nil;
    res
  else
    xs = f(d, n - 1);
    res = new d Cons(n, xs);
    mark(res, d); # Does nothing
    trim(xs, d);
    res
```

Unclear how to infer these parameters using just the pointer metas.
Best option is likely the queue gc thing.

Wait, queue won't work either: can't just mark things that are in scope.
e.g.

```bash
f(xs) =
  ys = new Cons(1, xs);
  xs

g(zs, ws) =
  f(zs);
  f(ws)
```

if gc happens on `new` inside `f(ws)`, queue contains `ys` from `f(zs)`
but only `ws` is in scope. Without `zs` to trace and keep `tail(ys)` alive,
will end up erroneously freeing `zs`.

So need accumulate things that might be freeable d and things that are
keeping them alive l. As you go back up the call stack, some things in l
might get added to d. In that case they should be removed from l.
So things get moved from l to d as you go back up the call stack.

Let s be the stuff in scope. The gc pass should
free d - l

Issues:
- Mutation?

Need C++ structures:
- `bitmap`: operations on b-tree bitmap thing
- `pool<N>`: mmap'd arena for items of size N + live/dead bitmap