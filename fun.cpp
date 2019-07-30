#define is(...) { static __VA_ARGS__ value; }
#define forall(...) template<__VA_ARGS__> struct
#define _ typename
#define fun template

template<_ t> using E = decltype(t::value);

// -------------------- Arithmetic --------------------

// type Bool = T | F
struct T {};
struct F {};

// type Nat = Z | S Nat
struct Z {};
forall(_ n) S {};

// type Pair = P _ _
forall(_ x, _ y) P {};

// nat2int : Nat -> unsigned int
forall(_ n) nat2int {};

// nat2int 0 = 0
forall() nat2int<Z> { static unsigned int const value = 0; };

// nat2int (S n) = 1 + nat2int n
forall(_ n) nat2int<S<n>> { static unsigned int const value = 1 + nat2int<n>::value; };

// suc : Nat -> Nat; suc n = S n
forall(_ n) suc is (S<n>);

// add : Nat -> Nat -> Nat
forall(_ m, _ n) add;
forall(_ n) add<Z, n> is (n);
forall(_ m, _ n) add<S<m>, n> is (S<E<add<m, n>>>);

// mul : Nat -> Nat -> Nat
forall(_ m, _ n) mul;
forall(_ n) mul<Z, n> is (Z);
forall(_ m, _ n) mul<S<m>, n> is (E<add<n, E<mul<m, n>>>>);

// eq : Nat -> Nat -> Bool
forall(_ m, _ n) eq;
forall() eq<Z, Z> is (T);
forall(_ m, _ n) eq<S<m>, S<n>> is (E<eq<m, n>>);
forall(_ m) eq<S<m>, Z> is (F);
forall(_ n) eq<Z, S<n>> is (F);

// type List = N | C _ List
struct N {};
forall(_ x, _ xs) C {};

// app : List -> List -> List
forall(_ xs, _ ys) app;
forall(_ ys) app<N, ys> is (ys);
forall(_ x, _ xs, _ ys) app<C<x, xs>, ys> is (C<x, E<app<xs, ys>>>);

// rev : List -> List
forall(_ xs) rev;
forall() rev<N> is (N);
forall(_ x, _ xs) rev<C<x, xs>> is (E<app<E<rev<xs>>, C<x, N>>>);

// map : (_ -> _) -> List -> List
forall(fun<_> _ f, _ xs) map;
forall(fun<_> _ f) map<f, N> is (N);
forall(fun<_> _ f, _ x, _ xs) map<f, C<x, xs>> is (C<E<f<x>>, E<map<f, xs>>>);

// foldr : (_ -> _ -> _) -> _ -> List -> List
forall(fun<_, _> _ f, _ e, _ xs) foldr;
forall(fun<_, _> _ f, _ e) foldr<f, e, N> is (e);
forall(fun<_, _> _ f, _ e, _ x, _ xs) foldr<f, e, C<x, xs>> is (E<f<x, E<foldr<f, e, xs>>>>);

// concat : List -> List; concat xs = foldr app [] xs
forall(_ xs) concat is (E<foldr<app, N, xs>>);

// replicate : Nat -> _ -> List
forall(_ n, _ x) replicate;
forall(_ x) replicate<Z, x> is (N);
forall(_ n, _ x) replicate<S<n>, x> is (C<x, E<replicate<n, x>>>);

// zip : List -> List -> List
forall(_ xs, _ ys) zip;
forall(_ xs) zip<xs, N> is (xs);
forall(_ ys) zip<N, ys> is (ys);
forall(_ x, _ xs, _ y, _ ys) zip<C<x, xs>, C<y, ys>> is (C<P<x, y>, E<zip<xs, ys>>>);

// ----------------------------------------

#include <cstdio>
template<_ t> void pretty() { puts(__PRETTY_FUNCTION__); }

int main() {
  // eq (2 + 2) 4
  pretty<E<eq<E<add<S<S<Z>>, S<S<Z>>>>, S<S<S<S<Z>>>>>>>();

  // eq (2 + 2) 3
  pretty<E<eq<E<add<S<S<Z>>, S<S<Z>>>>, S<S<S<Z>>>>>>();

  // mul (2 + 2) (3 + 3)
  using two = S<S<Z>>;
  using three = S<S<S<Z>>>;
  printf("mul (2 + 2) (3 + 3) = %d\n",
    nat2int<E<mul<E<add<two, two>>, E<add<three, three>>>>>::value);

  // rev [1, T, 3] = [3, T, 1]
  using one = S<Z>;
  pretty<E<rev<C<one, C<T, C<three, N>>>>>>();

  // map suc [1, 2, 3] = [2, 3, 4]
  pretty<E<map<suc, C<one, C<two, C<three, N>>>>>>();

  // foldr add 0 [1, 2, 3] = 6
  pretty<E<foldr<add, Z, C<one, C<two, C<three, N>>>>>>();

  // concat [[1, 2, 3], [2, 3, 4]] = [1, 2, 3, 2, 3, 4]
  using xs = C<one, C<two, C<three, N>>>;
  using ys = E<map<suc, xs>>;
  pretty<E<concat<C<xs, C<ys, N>>>>>();

  // concat (replicate 6) [1, 2, 3] = [...]
  pretty<E<concat<E<replicate<S<S<S<S<S<S<Z>>>>>>, xs>>>>>();
}
