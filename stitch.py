def mk():
  memo = {}
  strs = []
  def lit(s):
    if s not in memo:
      memo[s] = len(strs)
      strs.append(s)
    return memo[s]
  def nab():
    n = len(strs)
    strs.append("'" + str(n))
    return n
  def pp_lit(n):
    return strs[n] if n < len(strs) else "'" + str(n)
  return lit, nab, pp_lit
# lit : string -> lit
# nab(la) : unit -> fresh lit
# pp_lit : lit -> string
lit, nab, pp_lit = mk()
del mk

# fresh unification variable
class Var:
  def __init__(self):
    self.a = self

# log = list of thunks that will undo destructive updates (aka trail)
# f : action = log -> generator of None
def run(f):
  log = []
  g = f(log)
  next(g) # force at least one success
  return log, g

# unify : term, term -> action
def unify(l, r):
  def nop(_): pass
  class No(Exception): pass
  # set_l x stores x in l's parent location
  #   i.e. pointer graph goes (p -> l) x ==set_l x==> (p -> x) l
  #   l isn't destroyed, but just isolated from the old reference graph
  def unify_(log, l, r, set_l, set_r):
    # helpers
    def set_var(v):
      def res(x):
        v.a = x
      return res
    def set_arr(arr, i):
      def res(x):
        arr[i] = x
      return res
    def inst(v, e):
      log.append(lambda: set_var(v)(v))
      v.a = e
    def cyc(v, e, set_e):
      log.append(lambda: set_e(e))
      set_e(v)
      unify_(log, v.a, e, set_var(v), nop)
    # simple cases
    if l is r: return
    elif type(l) is type(r) is int and l == r: return
    elif type(l) is type(r) is list and len(l) == len(r):
      for i, (x, y) in enumerate(zip(l, r)):
        unify_(log, x, y, set_arr(l, i), set_arr(r, i))
    # instantiate variables
    elif type(l) is Var and l is l.a: inst(l, r)
    elif type(r) is Var and r is r.a: inst(r, l)
    # follow var -> var links (find representative)
    elif type(l) is Var is type(l.a): unify_(log, l.a, r, set_var(l), set_r)
    elif type(r) is Var is type(r.a): unify_(log, r.a, l, set_var(r), set_l)
    # var ~ non-var with var -> term:
    # - store var in non-var's parent pointer location
    # - check term ~ non-var
    # this allows for unification of cyclic structures
    elif type(l) is Var: cyc(l, r, set_r)
    elif type(r) is Var: cyc(r, l, set_l)
    else: raise No
  def res(log):
    try: yield unify_(log, l, r, nop, nop)
    except No: pass
  return res

# revert all actions taken from log[n] onwards
def undo(log, n=0):
  while len(log) > n:
    log.pop()()

# conj, disj : action .. -> action
def conj(*fs):
  def res(log):
    def go(fs):
      if len(fs) == 0:
        yield
      else:
        f, *fs = fs
        n = len(log)
        for _ in f(log):
          yield from go(fs)
          undo(log, n=n)
    return go(fs)
  return res
def disj(*fs):
  def res(log):
    n = len(log)
    for f in fs:
      yield from f(log)
      undo(log, n=n)
  return res

def zonk(t, verbose=False):
  done = set()
  def go(t):
    nonlocal done
    if type(t) is int: return pp_lit(t)
    elif type(t) is Var and t in done: return hex(id(t))
    elif type(t) is Var:
      done |= {t}
      return (hex(id(t)), go(t.a)) if verbose else go(t.a)
    elif type(t) is list: return [go(x) for x in t]
  return go(t)

def pp_zonked(t):
  return t if type(t) is str else '[' + ' '.join(map(pp_zonked, t)) + ']'

def test_app():
  # nil ++ XS = XS
  # (X :: XS) ++ YS = (X :: ZS) <== XS ++ YS = ZS
  go = lambda t: lambda log: disj(
    (lambda xs: unify(t, [lit('nil'), lit('++'), xs, lit('='), xs]))(Var()),
    (lambda x, xs, ys, zs: conj(
      unify(t, [[x, lit('::'), xs], lit('++'), ys, lit('='), [x, lit('::'), zs]]),
      go([xs, lit('++'), ys, lit('='), zs])))
      (Var(), Var(), Var(), Var()),
  )(log)
  nil = lit('nil')
  cons = lambda h, t: [h, lit('::'), t]
  # ('100 :: ('101 :: nil)) ~ XS
  xs = Var()
  log, _ = run(unify(cons(100, cons(101, nil)), xs))
  print(pp_zonked(zonk(xs)))
  undo(log)
  print(pp_zonked(zonk(xs)))
  print()
  # XS ~ ('100 :: XS)
  # YS ~ ('100 :: (Z :: YS))
  # ZS ~ YS
  # ----------------------
  # Z ~ 100
  # YS ~ ('100 :: XS)
  xs, ys, z = Var(), Var(), Var()
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(z)))
  log, _ = run(conj(
    unify(xs, cons(100, xs)),
    unify(ys, cons(100, cons(z, ys))),
    unify(xs, ys),
  ))
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(z)))
  undo(log)
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(z)))
  print()
  # (98 :: (99 :: nil)) ++ (100 :: (101 :: nil)) = (98 :: XS)
  # ---------------------------------------------------------
  # XS ~ (99 :: (100 :: (101 :: nil)))
  xs = Var()
  print(pp_zonked(zonk(xs)))
  log, _ = run(go([
    cons(98, cons(99, nil)), lit('++'), cons(100, cons(101, nil)),
    lit('='), cons(98, xs)]))
  print(pp_zonked(zonk(xs)))
  undo(log)
  print(pp_zonked(zonk(xs)))
  print()
  # (100 :: nil) ++ XS = XS
  # -----------------------
  # XS ~ (100 :: XS)
  xs = Var()
  print(pp_zonked(zonk(xs)))
  log, _ = run(go([
    cons(100, nil), lit('++'), xs, lit('='), xs]))
  print(pp_zonked(zonk(xs)))
  undo(log)
  print(pp_zonked(zonk(xs)))
  print()

def test_conj_back():
  # q a. q b. r c. r d.
  # p X Y <== q X, r Y.
  go = lambda t: lambda log: disj(
    unify(t, [lit('q'), lit('a')]),
    unify(t, [lit('q'), lit('b')]),
    unify(t, [lit('r'), lit('c')]),
    unify(t, [lit('r'), lit('d')]),
    (lambda x, y: conj(
      unify(t, [lit('p'), x, y]),
      go([lit('q'), x]),
      go([lit('r'), y])))
      (Var(), Var()),
  )(log)
  # p X Y ==>
  #   X ~ a, Y = c;
  #   X ~ a, Y = d;
  #   X ~ b, Y = c;
  #   X ~ b, Y = d
  x, y = Var(), Var()
  print(pp_zonked(zonk(x)), pp_zonked(zonk(y)))
  log = []
  for _ in go([lit('p'), x, y])(log):
    print(pp_zonked(zonk(x)), pp_zonked(zonk(y)))
  print(pp_zonked(zonk(x)), pp_zonked(zonk(y)))

if __name__ == '__main__':
  test_app()
  test_conj_back()
