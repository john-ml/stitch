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

# lit = string to lit
# nab(la) = fresh lit
# pp_lit = lit to string
lit, nab, pp_lit = mk()
del mk

# fresh unification variable
class Var:
  def __init__(self):
    self.a = self

def nop(_): pass

def set_var(v):
  def res(x):
    v.a = x
  return res

def set_arr(arr, i):
  def res(x):
    arr[i] = x
  return res

class No(Exception): pass

# log = list of thunks that will undo destructive updates
# set_l x stores x in l's parent location
#   i.e. pointer graph goes (p -> l) x ==set_l x==> (p -> x) l
#   l isn't destroyed, but just isolated from the old reference graph
def unify_(log, l, r, set_l, set_r):
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

unify = lambda l, r: lambda log: unify_(log, l, r, nop, nop)

def undo(log):
  for f in log: f()

def conj(*fs):
  def res(log):
    for f in fs:
      f(log)
  return res

def disj(*fs):
  def res(_):
    log = []
    for f in fs:
      try: f(log); break
      except No: undo(log)
    else: raise No
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

def run(f):
  log = []
  return log, f(log)

def test():
  def app(go, t):
    return disj(
      (lambda xs: unify(t, [lit('nil'), lit('++'), xs, lit('='), xs]))(Var()),
      (lambda x, xs, ys, zs: conj(
         unify(t, [[x, lit('::'), xs], lit('++'), ys, lit('='), [x, lit('::'), zs]]),
         go([xs, lit('++'), ys, lit('='), zs])))
         (Var(), Var(), Var(), Var()),
    )

  go = lambda t: lambda log: app(go, t)(log)
  nil = lit('nil')
  cons = lambda h, t: [h, lit('::'), t]

  xs = Var()
  log, _ = run(unify(cons(100, cons(101, nil)), xs))
  print(zonk(xs))
  undo(log)
  print(zonk(xs))

  xs, ys, z = Var(), Var(), Var()
  log, _ = run(conj(
    unify(xs, cons(100, xs)),
    unify(ys, cons(100, cons(z, ys))),
    unify(xs, ys),
  ))
  print(zonk(xs), zonk(ys), zonk(z))

  xs = Var()
  print(run(go([
    cons(98, cons(99, nil)), lit('++'), cons(100, cons(101, nil)),
    lit('='), cons(98, xs)])))
  print(zonk(xs))

  xs = Var()
  print(run(go([
    cons(100, nil), lit('++'), xs, lit('='), xs])))
  print(zonk(xs))

test()
