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

Escape analysis can prove that certain pointers will be inaccessible at
scope end. All such pointers returned by `new` can be automatically
deallocated.

e.g. 

```bash
f() =
  xs as list(i32) = countdown(i);
  0
  # Escape analysis proves that no pointer in xs is accessible after this
  # function returns, so we should be able to recursively deallocate all of
  # xs.

countdown(i) list(i32) =
  if i == 0 then new Nil else new Cons(i, countdown(i - 1))
```

But analysis can't definitively prove that things escape:

```bash
f(p bool, ys list(i32)) =
  xs = new Nil;
  if p then
    ys
    # xs can't escape, so deallocate
  else
    xs
    # xs 100% escapes, but analysis can only say that it might
```

Need to insert runtime code to trace from every other intermediate value in
scope that might be aliased inside `xs` (including the return value).

In this case, these values are `ys` and the return value `xs`.

Super naively, there are 3 steps:
1. Trace everything (traverse all of `xs` and all of `ys` and mark every pointer as 'escaping')
2. Deallocate any nonecaping pointers inside `xs` (`xs` itself is marked as escaping, so this step terminates immediately)
3. Retrace everything (both `xs` and `ys`) and reset every escaping mark

Need to stuff some extra info in low bits of pointers:
- `new`'d vs pointer to stack memory
- 'escaping' vs 'non-escaping'
