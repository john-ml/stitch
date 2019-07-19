#include <stdio.h>
#include <setjmp.h>
#include <functional>

// -------------------- TMP --------------------

template<int T> int factorial;
template<> int factorial<0> = 1;
template<int n> int factorial = n * factorial<n - 1>;

#define eval(...) decltype(__VA_ARGS__{}.a)

// Linked list of integers
struct Nil {};
template<int h, typename t> struct Cons {};

// head (h : t) = h
template<typename xs> struct head;
template<int h, typename t> struct head<Cons<h, t>>{};

// tail (h : t) = t
template<typename xs> struct tail;
template<int h, typename t> struct tail<Cons<h, t>>{t a;};

// List append

template<typename xs, typename ys> struct app;

// app [] ys = ys
template<typename ys> struct app<Nil, ys>{ys a;};

// app (x : xs) ys = x : app xs ys
template<int x, typename xs, typename ys>
struct app<Cons<x, xs>, ys> {Cons<x, eval(app<xs, ys>)> a;};

template<typename T> void pretty(T _) { puts(__PRETTY_FUNCTION__); }

// List sum

template<typename xs> struct sum;

template<> struct sum<Nil>{int const a = 0;};
template<int x, typename xs>
struct sum<Cons<x, xs>>{int const a = x + sum<xs>{}.a;};

// List generator

template<int n> struct down_from;

template<> struct down_from<0>{Nil a;};
template<int n> struct down_from {Cons<n, eval(down_from<n-1>)> a;};

// List reverse

template<typename xs> struct rev;
template<> struct rev<Nil>{Nil a;};
template<int x, typename xs>
struct rev<Cons<x, xs>>{eval(app<eval(rev<xs>), Cons<x, Nil>>) a;};

// -------------------- Sum types --------------------

enum slist_tag {nil, cons};

template<typename T> struct slist {
  slist_tag is;
  union {
    struct {} nil;
    struct {T h; slist *t;} cons;
  } as;
};

template<typename T> struct slist0 {
  slist_tag is;
  union {
    struct {} nil;
    struct {
      T h;
      struct {
        slist_tag is;
        union {struct {} nil; struct {T h; slist0 *t;} cons;} as;
      } *t;
    } cons;
  } as;
};

template<typename T> using list = slist<T> *;
template<typename T> using list0 = slist0<T> *;

// -------------------- Traits --------------------

// class Eq a where eq :: a -> a -> Bool
template<typename T> bool eq(T a, T b) = delete;

// instance Eq a => Eq (List a) where ...
template<typename T> bool eq(list<T> a, list<T> b) {
  puts(__PRETTY_FUNCTION__);
  eq<T>(a->as.cons.h, b->as.cons.h);
}

// instance Eq Int where ...
template<> bool eq<int>(int a, int b) { puts(__PRETTY_FUNCTION__); }

// -------------------- set/longjmp --------------------

// f :: forall a b. (Bool, a, ..b) -> a
template<typename T> T f(bool p, T x, jmp_buf k) {
  if (p) longjmp(k, 1);
  return x;
}

// g :: forall a. (Bool, a) -> a
template<typename T> T g(bool p, T x) {
  jmp_buf k;
  if (setjmp(k)) {
    puts("longjmp.");
    return x;
  } else {
    printf("Calling f; ");
    T res = f<T>(p, x, k);
    puts("no longjmp.");
    return res;
  }
}


// -------------------- Closures --------------------

// Stack-allocated
template<typename T> void thrice(T f) { f(); f(); f(); }

// Heap-allocated
void twice(std::function<void()> f) { f(); f(); }

int main() {
  // Eq Int
  eq(1, 2);

  // Eq (List Int)
  {
    slist<int> x{cons, {.cons = {0, nullptr}}};
    slist<int> y{cons, {.cons = {1, nullptr}}};
    list<int> xs = &x;
    list<int> ys = &y;
    eq(xs, ys);
  }

  // No instance for Eq (List Char)
  {
    slist<char> x{cons, {.cons = {0, nullptr}}};
    slist<char> y{cons, {.cons = {1, nullptr}}};
    list<char> xs = &x;
    list<char> ys = &y;
    // eq(xs, ys);
  }

  // Equirecursive types => coercions necessary
  {
    slist0<int> x{cons, {.cons = {0, nullptr}}};
    slist0<int> y{cons, {.cons = {1, nullptr}}};
    list0<int> xs = &x;
    list0<int> ys = &y;
    eq(reinterpret_cast<list<int>>(xs), reinterpret_cast<list<int>>(ys));
  }

  // set/longjmp in template functions
  {
    printf("Got %c.\n", g<char>(true, 'a'));
    printf("Got %c.\n", g<char>(false, 'b'));
  }

  // Basic template metaprogram
  printf("factorial<%d> = %d\n", 10, factorial<10>);

  pretty(tail<Cons<0, Cons<1, Nil>>>{}.a);
  pretty(
    app<
      Cons<0, Cons<1, Cons<2, Nil>>>,
      Cons<3, Cons<4, Cons<5, Nil>>>>
    {}.a);
  printf("sum([0, 1, 2] ++ [3, 4, 5]) = %d\n",
    sum<eval(app<
      Cons<0, Cons<1, Cons<2, Nil>>>,
      Cons<3, Cons<4, Cons<5, Nil>>>>)>
    {}.a);
  pretty(down_from<20>{}.a);
  printf("sum [20,19..1] = %d\n", sum<eval(down_from<20>)>{}.a);
  printf("reverse [20,19..1] = "); pretty(rev<eval(down_from<20>)>{}.a);

  // Closures
  {
    int x = 0;
    thrice([&x](){x += 1; printf("x = %d\n", x);});
  }
}

/* Misc. notes

- new/delete call ctr/dtr. OK with continuations?
  Maybe heap-alloc closures will leak?
  Generally, POD is OK. Use static_assert(std::is_pod<T>::value) to check

- Each parametrically polymorphic function is compiled to a single C++ function
  template. Row polymorphism and function pointers/stack-allocated closures are
  erased too: {x T; R} and (T, ..) -> R both become a single C++ template type
  variable T. The stitch typechecker should prove that all field accesses and
  function calls are safe.

- Each `trait ..` declaration containing trait methods f1 .. fn is compiled to
  the 'declaration' of n deleted C++ template functions: for example,
  
    trait a num {__add__(a, a) -> a, __mul__(a, a) -> a}

  becomes

    template<typename A> A __add__(A, A) = delete;
    template<typename A> A __mul__(A, A) = delete;

  The trait name is erased.

- Each `impl ..` declaration containing definitions for trait methods f1 .. fn
  is compiled to the definition of n (possibly partially) specialized C++
  template functions. For example,

    impl i32 num {
      __add__(x, y) = ...
      __mul__(x, y) = ...
    }

    impl(a num) option(a) num {
      __add__(x, y) = ...
      __mul__(x, y) = ...
    }

  becomes

    template<> __add__<i32>(i32 x, i32 y) { ... }
    template<> __mul__<i32>(i32 x, i32 y) { ... }

    template<typename A> __add__(option<A> x, option<A> y) { ... }
    template<typename A> __mul__(option<A> x, option<A> y) { ... }

  The trait name and any prerequisites (e.g. `a num` in the `option(a) num`
  instance) are erased. The stitch typechecker should prove that, for any call
  to `__add__` or `__mul__`, there is a unique matching specialization (the
  C++ compiler can never select the wrong overload and will never select the
  deleted impl).

- Each function call f(x, ..) becomes f<T, ..>(reinterpret_cast<T>(x), ..) where
  T, .. are the proper type arguments for the polymorphic function f. The
  reinterpret casts are necessary because C++ doesn't understand equirecursion.
  The stitch typechecker should prove that all such casts are safe.

*/
