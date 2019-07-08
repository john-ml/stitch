# Stitch

A simple expression-oriented language.

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

If `e` is an expression and `s` is a statement, then `s; e` is an expression too:

```bash
triple_factorial(x) =
  res = factorial(x);
  res := factorial(res);
  res := factorial(res);
  res
```

Types can be manually specified:

```bash
double(x i32) i32 = x + x

factorial(n i32) i32 =
  if n <= 0 then 1 else n * factorial(n - 1)

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

`@` is used to create values of sum types:

```bash
checked_div(a i32, b i32) <none {}, some i32> =
  if b == 0 then none@{} else some @ a/b
```

`dyn` is used to create a heap-allocated value:

```bash
type list(A) = *<nil {}, cons {hd A, tl list(A)}>

countdown(n i32) list(i32) =
  if n < 0 then
    dyn nil@{}
  else
    dyn cons @ {hd = n, tl = countdown(n - 1)}
```

## Control flow

### Labelled expressions

In place of looping constructs, expressions can be labelled
and can refer to other labels in scope:

```bash
mut_factorial(n i32) i32 =
  res = 1;
  rec: # The if-expression below is labelled `rec`
    if n == 1 then 
      res
    else (
      res := res * n;
      n := n - 1;
      rec # Next iteration
    )
```

Expressions can only refer to labels in tail position, and a label is
in scope only within the expression that it is labelling.
(It's a lot like a restricted `goto` that has to yield a value).

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

The algorithm does not infer a column type, as that would imply that the `case`
expression could be non-exhaustive.

However, if we add a final catch-all pattern, then the algorithm _will_ infer
a column type, since it's possible for such a `case` expression to be
polymorphic in additional variant options while remaining exhaustive:

```bash
f[A, B, C](x <tag1 A, tag2 B; C>) bool =
  case x {
    tag1 _ -> true,
    tag2 _ -> false,
    _ -> true
  }
```

### Recursive types

Unlike ML/Haskell, Stitch allows recursive type aliases (and, in general,
supports equirecursive types rather than isorecursive ones). This
complicates type equality---for example, the following 5 types are considered
equivalent, because they all have the same 'infinite expansions':

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
type t1(R1, R2, R3, R4, R5, R6) = 
  *<lit i32,
    plus {a t1(R1, R2, R3, R4, R5, R6), b t1(R1, R2, R3, R4, R5, R6); R1}, 
    mult {a t1(R1, R2, R3, R4, R5, R6), b t1(R1, R2, R3, R4, R5, R6); R2}, 
    ifte {p t2(R1, R2, R3, R4, R5, R6), a t1(R1, R2, R3, R4, R5, R6), b t1(R1, R2, R3, R4, R5, R6); R3}>

type t2(R1, R2, R3, R4, R5, R6) =
  *<lit bool,
    and {a t2(R1, R2, R3, R4, R5, R6), b t2(R1, R2, R3, R4, R5, R6); R4},
    not {a t2(R1, R2, R3, R4, R5, R6); R5},
    leq {a t1(R1, R2, R3, R4, R5, R6), b t1(R1, R2, R3, R4, R5, R6); R6}>

eval_int_exp[R1, R2, R3, R4, R5, R6](e t1(R1, R2, R3, R4, R5, R6)) i32 = ...

eval_bool_exp[R1, R2, R3, R4, R5, R6](e t2(R1, R2, R3, R4, R5, R6)) bool = ...
```

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
type T = *<nil A, cons {tl T; R1}>
type R = *<nil {}, cons {hd int, tl R}>
zeros_like(xs T) R = ...
```

where `A` and `R1` are unsolved metavariables.
Then, when generalizing, all type aliases get extra type parameters corresponding to
each unsolved metavariable:

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

yields the constraints

```bash
type S =
  *<lit i32,
    shear {a S, b S, c S; R1},
    ifte {p T, a T, b T; R2}>

type T =
  *<lit bool,
    nand {a T, b T; R3},
    leq {a S, b S; R4}>

eval_int_exp(e S) i32 = ...

eval_bool_exp(e T) bool = ...
```

which, after closing over all free variables in the definitions
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
