import z3

def mk():
  memo = {}
  strs = []
  # lit : string -> atom
  def lit(s):
    if s not in memo:
      memo[s] = len(strs)
      strs.append(s)
    return memo[s]
  # nab(la) : unit -> fresh atom
  def nab():
    n = len(strs)
    strs.append("'" + str(n))
    return n
  # pp_lit : atom -> str
  def pp_lit(n):
    return strs[n] if n < len(strs) else "'" + str(n)
  return lit, nab, pp_lit
lit, nab, pp_lit = mk()
del mk

# fresh unification variable
class Var:
  def __init__(self):
    self.a = self # initially a self-loop

# constraint
class SMT:
  def __init__(self, a):
    self.a = a

# log = mutable list of thunks that will undo destructive updates (aka trail)
# solver = SMT solver
# f : action = log, solver -> generator of None
def run(f):
  log = []
  solver = z3.Solver()
  g = f(log, solver)
  next(g) # force at least one success
  return log, solver, g

# unify : term, term -> action
def unify(l, r):
  def nop(_): pass
  class No(Exception): pass
  # set_l x stores x in l's parent location
  #   i.e. pointer graph goes (p -> l) x ==set_l x==> (p -> x) l
  #   l isn't destroyed, but just isolated from the old reference graph
  def unify_(log, solver, l, r, set_l, set_r):
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
      go(v.a, e, set_var(v), nop)
    def go(l, r, set_l, set_r):
      # simple cases
      if l is r: return
      elif type(l) is type(r) is int and l == r: return
      elif type(l) is type(r) is list and len(l) == len(r):
        for i, (x, y) in enumerate(zip(l, r)):
          go(x, y, set_arr(l, i), set_arr(r, i))
      # instantiate variables
      elif type(l) is Var and l is l.a: inst(l, r)
      elif type(r) is Var and r is r.a: inst(r, l)
      # follow var -> var links (find representative)
      elif type(l) is Var is type(l.a): go(l.a, r, set_var(l), set_r)
      elif type(r) is Var is type(r.a): go(r.a, l, set_var(r), set_l)
      # var ~ non-var with var -> term:
      # - store var in non-var's parent pointer location
      # - check term ~ non-var
      # this allows for unification of cyclic structures
      elif type(l) is Var: cyc(l, r, set_r)
      elif type(r) is Var: cyc(r, l, set_l)
      # smt ~ smt: add equality constraint and check sat
      elif type(l) is type(r) is SMT:
        solver.add(l.a == r.a)
        if solver.check() != z3.sat: raise No # unknown => fail
      else: raise No
    return go(l, r, set_l, set_r)
  def res(log, solver):
    try: yield unify_(log, solver, l, r, nop, nop)
    except No: pass
  return res

# revert all actions taken from log[n] onwards
def undo(log, n):
  while len(log) > n:
    log.pop()()

# add : z3 formula .. -> action
def add(*fs):
  def res(log, solver):
    solver.add(*fs)
    if solver.check() == z3.sat: yield
  return res

# conj : action .. -> action
def conj(*fs):
  def res(log, solver):
    def go(fs):
      if len(fs) == 0:
        yield
      else:
        f, *fs = fs
        n = len(log)
        for _ in f(log, solver):
          solver.push()
          yield from go(fs)
          undo(log, n)
          solver.pop()
    return go(fs)
  return res

# disj : action .. -> action
def disj(*fs):
  def res(log, solver):
    n = len(log)
    for f in fs:
      solver.push()
      yield from f(log, solver)
      undo(log, n)
      solver.pop()
  return res

# program = term -> action
# trace : program -> program
def trace(f):
  def res(t):
    print(zonk(t))
    return f(t)
  return res

# expand out non-cyclic uvars
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
    elif type(t) is SMT: return str(t.a)
    else: raise ValueError(f"Can't zonk {t}")
  return go(t)

def pp_zonked(t):
  return t if type(t) is str else '[' + ' '.join(map(pp_zonked, t)) + ']'

def arr(*ts):
  res, *ts = ts
  while ts != []:
    h, *ts = ts
    res = [res, h]
  return res

# appending (potentially infinite) lists
def test_app():
  # nil ++ XS = XS
  # (X :: XS) ++ YS = (X :: ZS) <== XS ++ YS = ZS
  go = lambda t: lambda log, solver: disj(
    (lambda xs: unify(t, arr(lit('nil'), lit('++'), xs, lit('='), xs)))(Var()),
    (lambda x, xs, ys, zs: conj(
      unify(t, arr(
        arr(x, lit('::'), xs), lit('++'), ys, lit('='), arr(x, lit('::'), zs)
      )),
      go(arr(xs, lit('++'), ys, lit('='), zs))))
      (Var(), Var(), Var(), Var()),
  )(log, solver)
  nil = lit('nil')
  cons = lambda h, t: arr(h, lit('::'), t)
  # ('100 :: ('101 :: nil)) ~ XS
  xs = Var()
  log, _, _ = run(unify(cons(100, cons(101, nil)), xs))
  print(pp_zonked(zonk(xs)))
  undo(log, 0)
  print(pp_zonked(zonk(xs)))
  print()
  # ('100 :: ('101 :: nil)) ++ ('102 :: ('103 :: nil)) = XS
  xs = Var()
  log = []
  solver = z3.Solver()
  query = arr(
    cons(100, cons(101, nil)), lit('++'), cons(102, cons(103, nil)), lit('='), xs)
  for _ in go(query)(log, solver):
    print('xs :=', pp_zonked(zonk(xs)))
  print()
  # XS ~ ('100 :: XS)
  # YS ~ ('100 :: (Z :: YS))
  # ZS ~ YS
  # ----------------------
  # Z ~ 100
  # YS ~ ('100 :: XS)
  xs, ys, z = Var(), Var(), Var()
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(z)))
  log, _, _ = run(conj(
    unify(xs, cons(100, xs)),
    unify(ys, cons(100, cons(z, ys))),
    unify(xs, ys),
  ))
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(z)))
  undo(log, 0)
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(z)))
  print()
  # XS ~ ('100 :: ('100 :: XS))
  # YS ~ ('100 :: ('100 :: ('100 :: ZS)))
  # ZS ~ ('100 :: YS)
  # XS ~ YS
  xs, ys, zs = Var(), Var(), Var()
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(zs)))
  log, _, _ = run(conj(
    unify(xs, cons(100, cons(100, xs))),
    unify(ys, cons(100, cons(100, cons(100, zs)))),
    unify(zs, cons(100, ys)),
    unify(xs, ys),
  ))
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(zs)))
  undo(log, 0)
  print(pp_zonked(zonk(xs)), pp_zonked(zonk(ys)), pp_zonked(zonk(zs)))
  print()
  # (98 :: (99 :: nil)) ++ (100 :: (101 :: nil)) = (98 :: XS)
  # ---------------------------------------------------------
  # XS ~ (99 :: (100 :: (101 :: nil)))
  xs = Var()
  print(pp_zonked(zonk(xs)))
  log, _, _ = run(go(arr(
    cons(98, cons(99, nil)), lit('++'), cons(100, cons(101, nil)),
    lit('='), cons(98, xs)
  )))
  print(pp_zonked(zonk(xs)))
  undo(log, 0)
  print(pp_zonked(zonk(xs)))
  print()
  # (100 :: nil) ++ XS = XS
  # -----------------------
  # XS ~ (100 :: XS)
  xs = Var()
  print(pp_zonked(zonk(xs)))
  log, _, _ = run(go(arr(
    cons(100, nil), lit('++'), xs, lit('='), xs
  )))
  print(pp_zonked(zonk(xs)))
  undo(log, 0)
  print(pp_zonked(zonk(xs)))
  print()

# basic backtracking to find more solutions
def test_conj_back():
  # q a. q b. r c. r d.
  # p X Y <== q X, r Y.
  go = lambda t: lambda log, solver: disj(
    unify(t, arr(lit('q'), lit('a'))),
    unify(t, arr(lit('q'), lit('b'))),
    unify(t, arr(lit('r'), lit('c'))),
    unify(t, arr(lit('r'), lit('d'))),
    (lambda x, y: conj(
      unify(t, arr(lit('p'), x, y)),
      go(arr(lit('q'), x)),
      go(arr(lit('r'), y))
    ))(Var(), Var()),
  )(log, solver)
  # p X Y ==>
  #   X ~ a, Y = c;
  #   X ~ a, Y = d;
  #   X ~ b, Y = c;
  #   X ~ b, Y = d
  x, y = Var(), Var()
  print(pp_zonked(zonk(x)), pp_zonked(zonk(y)))
  log = []
  for _ in go(arr(lit('p'), x, y))(log, z3.Solver()):
    print(pp_zonked(zonk(x)), pp_zonked(zonk(y)))
  print(pp_zonked(zonk(x)), pp_zonked(zonk(y)))
  print()

# constraint solving
def test_smt():
  # fac 0 = 1.
  # fac {N + 1} = P <== {N >= 0}, {(N + 1) * M = P}, fac N = M.
  go = lambda t: lambda log, solver: disj(
    unify(t, arr(lit('fac'), SMT(0), lit('='), SMT(1))),
    (lambda n, m, p: conj(
      unify(t, arr(lit('fac'), SMT(n.a + 1), lit('='), p)),
      add(n.a >= 0),
      add((n.a + 1) * m.a == p.a),
      go(arr(lit('fac'), n, lit('='), m)),
    ))(SMT(z3.Int(nab())), SMT(z3.Int(nab())), SMT(z3.Int(nab())))
  )(log, solver)
  # fac 100! X
  # ----------
  # {X = 100!}
  x = SMT(z3.Int('x'))
  log = []
  solver = z3.Solver()
  for _ in go(arr(lit('fac'), SMT(100), lit('='), x))(log, solver):
    print(
      solver, solver.check(), 'x =', 
      (solver.model()[x.a] if solver.check() == z3.sat else None),
    )
  print(solver)
  print()
  # fac X 720
  # ---------
  #   X = 7
  x = SMT(z3.Int('x'))
  log = []
  solver = z3.Solver()
  for _ in go(arr(lit('fac'), x, lit('='), SMT(720*7)))(log, solver):
    print(
      solver, solver.check(), 'x =', 
      (solver.model()[x.a] if solver.check() == z3.sat else None),
    )
  print(solver)
  print()
  # enumerating all solutions to constraints
  # pythag A B C N <==
  #   {0 < A}, {A <= B}, {B <= C}, {C <= N}
  #   {A^2 + B^2 = C^2}.
  go = lambda t: lambda log, solver: (lambda a, b, c, n: conj(
    unify(t, arr(lit('pythag'), a, b, c, n)),
    add(0 < a.a, a.a <= b.a, b.a <= c.a, c.a <= n.a, a.a**2 + b.a**2 == c.a**2)
  )(log, solver))(
    SMT(z3.Int(nab())), SMT(z3.Int(nab())),
    SMT(z3.Int(nab())), SMT(z3.Int(nab()))
  )
  a = SMT(z3.Int('a'))
  b = SMT(z3.Int('b'))
  c = SMT(z3.Int('c'))
  log = []
  solver = z3.Solver()
  for _ in go(arr(lit('pythag'), a, b, c, SMT(30)))(log, solver):
    solver.push()
    while solver.check() == z3.sat:
      m = solver.model()
      ma, mb, mc = m[a.a], m[b.a], m[c.a]
      print(f'a = {ma}, b = {mb}, c = {mc}')
      solver.add(z3.Not(z3.And(a.a == ma, b.a == mb, c.a == mc)))
    solver.pop()
  print()
  # cbrt {N * N * N} = N.
  go = lambda t: (lambda n: conj(
    unify(t, arr(lit('cbrt'), SMT(n.a * n.a * n.a), lit('='), n))
  ))(SMT(z3.Int(nab())))
  n = SMT(z3.Int('n'))
  log = []
  solver = z3.Solver()
  for _ in go(arr(lit('cbrt'), SMT(2019 * 2019 * 2019), lit('='), n))(log, solver):
    solver.push()
    while solver.check() == z3.sat:
      m = solver.model()
      mn = m[n.a]
      print(f'n = {mn}')
      solver.add(n.a != mn)
    solver.pop()
  print()

def parse(s):
  from lark import Lark, Transformer
  import lark
  grammar = Lark('''
  %import common.WS
  %ignore WS
  %ignore COMMENT
  start: (fact | query)*
  fact: term ("<==" _rhs)? "."
  query: _rhs "?"
  _rhs: rhs_dis
  ?rhs_dis: rhs_con | rhs_dis ";" rhs_con
  ?rhs_con: _rhs_atom | rhs_con "," _rhs_atom
  _rhs_atom: term | "(" _rhs ")"

  term: _atom | term _atom
  _atom: LIT | VAR | smt | "(" term ")"

  smt: "{" (smt_form | smt_rel) "}"
  ?smt_form: smt_or | smt_form "->" smt_or
  ?smt_or: smt_and | smt_or "\\\\/" smt_and
  ?smt_and: smt_rel | smt_and "/\\\\" smt_rel
  ?smt_rel: smt_sum | smt_sum REL smt_sum | "(" smt_form ")" | NOT smt_rel
  ?smt_sum: smt_prod | smt_sum ADD smt_prod
  ?smt_prod: _smt_atom | smt_prod MUL _smt_atom
  _smt_atom: VAR | "(" smt_rel ")"

  NOT: "~"
  REL: "=" | "/=" | "<" | ">" | "<=" | ">="
  ADD: "+" | "-"
  MUL: "*" | "/"

  LIT: /(?!<==)[^-(){},;.?A-Z\\s][^(){},;.?\\s]*/
  VAR: /[A-Z][a-zA-Z0-9_'-]*/ | "_"

  COMMENT: /--[^\\n]+/
  ''')
  def translate(children):
    def go(child):
      if type(child) is not lark.lexer.Token: return child
      elif child.type == 'LIT': return lit(child.value)
      else: return child.value
    return list(map(go, children))
  def uninfix(xs): return xs[1], xs[0], xs[2]
  class Translate(Transformer):
    def start(self, statements):
      facts = []
      queries = []
      for ty, terms in statements:
        (facts if ty == 'fact' else queries).append(terms)
      return 'program', facts, queries
    def fact(self, terms): return ('fact', terms)
    def query(self, terms): return ('query', terms)
    def term(self, children):
      if len(children) == 1: return translate(children)[0]
      else: return translate(children)
    def smt(self, children): return 'smt', translate(children)[0]
    def smt_form(self, children): return ('implies', *translate(children))
    def smt_or(self, children): return ('or', *translate(children))
    def smt_and(self, children): return ('and', *translate(children))
    def smt_sum(self, children): return uninfix(translate(children))
    def smt_prod(self, children): return uninfix(translate(children))
    def smt_rel(self, children):
      if len(children) == 3: return uninfix(translate(children))
      else: return 'not', translate(children)[1]
    def rhs_con(self, children): return ('con', *translate(children))
    def rhs_dis(self, children): return ('dis', *translate(children))
  return Translate().transform(grammar.parse(s))

def match(x):
  def go(*arms):
    pairs = list(zip(*[iter(arms)] * 2))
    def safe(f):
      try: f(); return True
      except: return False
    for pat, arm in pairs:
      if pat is any: return arm(x)
      elif type(pat) is type and type(x) is pat: return arm(x)
      elif type(pat) is type(lambda: None) and pat(x): return arm(x)
      elif safe(lambda: x[0]) and x[0] == pat: return arm(*x[1:])
    else:
      raise ValueError('No matching pattern for ' + str(x))
  return go

def throw(s): raise ValueError(s)

def compile(p):
  def cc_smt(form):
    pass # TODO
  # instantiate variables with fresh Var()s
  def inst(terms):
    names = {}
    def get(x):
      if x not in names: names[x] = nab()
      return names[x]
    def go(term):
      return match (term) (
        list, lambda terms: [inst(t) for t in terms],
        str, get,
        # TODO
      )
    return go(terms)
  def cc_facts(facts):
    def cc_fact(fact):
      if len(fact) == 1:
        return lambda _, t: unify(t, inst(fact))
      else:
        def res(rec, t):
          fact = inst(fact)
          hd, tl = fact[0], fact[1:]
          return conj(unify(t, hd), *map(rec, tl))
        return res
    facts = [cc_fact(f) for f in facts]
    rec = lambda t: lambda log, solver: disj(*(f(rec, t) for f in facts))(log, solver)
    return res
  return match (p) (
    'program', lambda facts, queries:
      (cc_facts(facts), cc_queries(queries)),
  )

if __name__ == '__main__':
  test_app()
  test_conj_back()
  test_smt()

  print(parse('''
    -- Concatenate two lists
    nil ++ XS = XS.
    (X :: XS) ++ YS = (X :: ZS) <== XS ++ YS = ZS.

    -- Map a function over a list
    map _ nil = nil.
    map F (X :: XS) = (Y :: YS) <==
      F X = Y,
      map F XS = YS.
  '''))
  print()

  print(parse('''
    -- Square root
    sqrt {N * N} = {N}.

    -- Conjunction
    conj {F} {G} <== {F /\\ G}.
    conj {F} {G} <== F, G.

    f {F} {G} <== (F; G), G; F.

    -- Sample query
    p X?
  '''))
  print()
